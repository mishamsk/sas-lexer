"""Version and changelog tooling for the two release trains (stdlib only, Python 3.11+).

Trains:
  rust  crates sas-lexer and sas-lexer-macro; version in [workspace.package] of Cargo.toml;
        CHANGELOG-RUST.md; tags rust-v<version>
  py    the sas-lexer Python package; version in crates/sas-lexer-py/Cargo.toml, mirrored in
        pyproject.toml and src/sas_lexer/__init__.py; CHANGELOG-PY.md; tags py-v<version>

Versions use Cargo spelling (1.0.0-beta.2); wheels and PyPI use the PEP 440 form (1.0.0b2).

Commands:
  prep TRAIN VERSION       set the train's version and turn the changelog's Unreleased section into
                           a dated VERSION entry (lockfiles are left to the caller)
  check TRAIN [--tag TAG]  the train's version sources agree; with TAG, also TAG is the train tag
                           for that version and the changelog has a dated entry for it
  dev NUMBER               py only: set pyproject.toml and __version__ to <PEP 440>.dev<NUMBER>
                           (TestPyPI builds)
  artifacts VERSION DIR    DIR holds one sas_lexer VERSION sdist, its wheels and nothing else

check and dev print key=value lines for $GITHUB_OUTPUT. Errors go to stderr with exit status 1.
"""

import argparse
import datetime
import re
import sys
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
REPO_URL = "https://github.com/mishamsk/sas-lexer"
DIST_NAME = "sas_lexer"

# X.Y.Z or X.Y.Z-(alpha|beta|rc).N. Other forms are rejected rather than guessed, because their
# PEP 440 form would be ambiguous or not uploadable (PyPI rejects +local versions).
VERSION = re.compile(
    r"(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)"
    r"(?:-(?P<label>alpha|beta|rc)\.(?P<number>0|[1-9]\d*))?"
)
PEP440_LABEL = {"alpha": "a", "beta": "b", "rc": "rc"}
LABEL_RANK = {"alpha": 0, "beta": 1, "rc": 2, None: 3}

SECTIONS = ("Added", "Changed", "Deprecated", "Removed", "Fixed", "Security")
UNRELEASED = "## [Unreleased] - ReleaseDate"
UNRELEASED_TEMPLATE = UNRELEASED + "\n\n" + "".join(f"### {s}\n-\n\n" for s in SECTIONS)


class ReleaseError(Exception):
    pass


@dataclass(frozen=True)
class VersionSource:
    path: str
    # Group 1: text before the version, group 2: the version, group 3: the closing quote.
    pattern: re.Pattern[str]
    # Accept the PEP 440 spelling too (Python metadata files).
    pep440_ok: bool = False


@dataclass(frozen=True)
class Train:
    tag_prefix: str
    changelog: str
    sources: tuple[VersionSource, ...]


TRAINS = {
    "rust": Train(
        "rust-v",
        "CHANGELOG-RUST.md",
        (
            VersionSource(
                "Cargo.toml",
                re.compile(
                    r'(\[workspace\.package\]\r?\n(?:[^\[\r\n].*\n|\r?\n)*?version = ")([^"]+)(")'
                ),
            ),
        ),
    ),
    "py": Train(
        "py-v",
        "CHANGELOG-PY.md",
        (
            VersionSource(
                "crates/sas-lexer-py/Cargo.toml",
                re.compile(r'(\[package\]\r?\n(?:[^\[\r\n].*\n|\r?\n)*?version = ")([^"]+)(")'),
            ),
            VersionSource(
                "pyproject.toml",
                re.compile(r'(\[project\]\r?\nname = "sas-lexer"\r?\nversion = ")([^"]+)(")'),
                pep440_ok=True,
            ),
            VersionSource(
                "src/sas_lexer/__init__.py",
                re.compile(r'^(__version__ = ")([^"]+)(")', re.MULTILINE),
                pep440_ok=True,
            ),
        ),
    ),
}


def parse(version: str) -> re.Match[str]:
    m = VERSION.fullmatch(version)
    if m is None:
        raise ReleaseError(
            f"unsupported version {version!r}: expected X.Y.Z or X.Y.Z-(alpha|beta|rc).N"
        )
    return m


def to_pep440(version: str) -> str:
    m = parse(version)
    release = f"{m['major']}.{m['minor']}.{m['patch']}"
    if m["label"] is None:
        return release
    return f"{release}{PEP440_LABEL[m['label']]}{m['number']}"


def sort_key(version: str) -> tuple[int, ...]:
    m = parse(version)
    return (
        int(m["major"]),
        int(m["minor"]),
        int(m["patch"]),
        LABEL_RANK[m["label"]],
        int(m["number"] or 0),
    )


def read(root: Path, rel: str) -> str:
    # newline="" keeps CRLF line endings (Windows checkouts) intact on write-back.
    with (root / rel).open(encoding="utf-8", newline="") as f:
        return f.read()


def write(root: Path, rel: str, text: str) -> None:
    with (root / rel).open("w", encoding="utf-8", newline="") as f:
        f.write(text)


def find(source: VersionSource, text: str) -> re.Match[str]:
    matches = list(source.pattern.finditer(text))
    if len(matches) != 1:
        raise ReleaseError(f"expected one version in {source.path}, found {len(matches)}")
    return matches[0]


def check(root: Path, train: Train) -> str:
    """Return the train version once all its sources agree."""
    primary, *mirrors = train.sources
    version = find(primary, read(root, primary.path))[2]
    pep440 = to_pep440(version)
    for source in mirrors:
        found = find(source, read(root, source.path))[2]
        if found != version and not (source.pep440_ok and found == pep440):
            raise ReleaseError(
                f"{source.path} has version {found!r}, {primary.path} has {version!r}"
            )
    return version


def check_tag(root: Path, train: Train, tag: str) -> str:
    version = check(root, train)
    if tag != train.tag_prefix + version:
        raise ReleaseError(f"tag {tag!r} does not match version {version!r}")
    heading = re.compile(
        rf"^## \[{re.escape(version)}\] - \d{{4}}-\d{{2}}-\d{{2}}\r?$", re.MULTILINE
    )
    if not heading.search(read(root, train.changelog)):
        raise ReleaseError(f"{train.changelog} has no dated '## [{version}] - YYYY-MM-DD' entry")
    return version


def release_changelog(text: str, train: Train, version: str, today: str) -> str:
    start = text.find(UNRELEASED + "\n")
    if start < 0:
        raise ReleaseError(f"no '{UNRELEASED}' section")
    end = text.find("\n## [", start + len(UNRELEASED))
    end = len(text) if end < 0 else end + 1
    body = text[start + len(UNRELEASED) : end]
    # Drop subsections left with the "-" placeholder only.
    body = re.sub(r"### \w+\n-[ \t]*\n\n?", "", body)
    if "\n### " not in body:
        raise ReleaseError("the Unreleased section has no entries")
    released = f"{UNRELEASED_TEMPLATE}## [{version}] - {today}{body}"
    text = text[:start] + released + text[end:]

    tag = train.tag_prefix + version
    link = re.compile(r"^\[Unreleased\]: (\S+/compare/)(\S+)\.\.\.HEAD$", re.MULTILINE)
    m = link.search(text)
    if m is None:
        raise ReleaseError("no '[Unreleased]: .../compare/<tag>...HEAD' link")
    links = f"[Unreleased]: {m[1]}{tag}...HEAD\n[{version}]: {m[1]}{m[2]}...{tag}"
    return text[: m.start()] + links + text[m.end() :]


def prep(root: Path, train: Train, version: str, today: str) -> str:
    current = check(root, train)
    if sort_key(version) <= sort_key(current):
        raise ReleaseError(f"{version} is not newer than the current version {current}")
    changelog = read(root, train.changelog)
    if f"## [{version}]" in changelog:
        raise ReleaseError(f"{train.changelog} already has a {version} entry")
    # Compute every change before writing any file, so a failure leaves the tree untouched.
    updates = {train.changelog: release_changelog(changelog, train, version, today)}
    for source in train.sources:
        text = read(root, source.path)
        m = find(source, text)
        updates[source.path] = text[: m.start(2)] + version + text[m.end(2) :]
    for rel, text in updates.items():
        write(root, rel, text)
    return check(root, train)


def set_dev(root: Path, number: str) -> str:
    train = TRAINS["py"]
    version = check(root, train)
    if not re.fullmatch(r"0|[1-9]\d*", number):
        raise ReleaseError(f"dev number must be a non-negative integer, got {number!r}")
    dev = f"{to_pep440(version)}.dev{number}"
    for source in train.sources[1:]:
        text = read(root, source.path)
        m = find(source, text)
        write(root, source.path, text[: m.start(2)] + dev + text[m.end(2) :])
    return dev


def check_artifacts(version: str, directory: Path) -> list[str]:
    names = sorted(p.name for p in directory.iterdir()) if directory.is_dir() else []
    sdist = f"{DIST_NAME}-{version}.tar.gz"
    wheel_prefix = f"{DIST_NAME}-{version}-"
    wrong = [
        n for n in names if n != sdist and not (n.startswith(wheel_prefix) and n.endswith(".whl"))
    ]
    if wrong:
        raise ReleaseError(f"not {DIST_NAME} {version} artifacts: {', '.join(wrong)}")
    if sdist not in names or not any(n.endswith(".whl") for n in names):
        raise ReleaseError(f"expected the {sdist} sdist and at least one wheel in {directory}")
    return names


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("--root", type=Path, default=ROOT)
    sub = parser.add_subparsers(dest="command", required=True)
    p = sub.add_parser("prep")
    p.add_argument("train", choices=TRAINS)
    p.add_argument("version")
    p = sub.add_parser("check")
    p.add_argument("train", choices=TRAINS)
    p.add_argument("--tag")
    sub.add_parser("dev").add_argument("number")
    p = sub.add_parser("artifacts")
    p.add_argument("version")
    p.add_argument("directory", type=Path)
    args = parser.parse_args(argv)

    try:
        if args.command == "prep":
            today = datetime.datetime.now(datetime.UTC).date().isoformat()
            version = prep(args.root, TRAINS[args.train], args.version, today)
            print(f"version={version}")
        elif args.command == "check":
            train = TRAINS[args.train]
            if args.tag is None:
                version = check(args.root, train)
            else:
                version = check_tag(args.root, train, args.tag)
            print(f"version={version}\npep440={to_pep440(version)}")
        elif args.command == "dev":
            print(f"dev={set_dev(args.root, args.number)}")
        else:
            names = check_artifacts(args.version, args.directory)
            print(f"{len(names)} artifacts for {DIST_NAME} {args.version}:", *names, sep="\n  ")
    except (ReleaseError, OSError) as e:
        print(f"error: {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
