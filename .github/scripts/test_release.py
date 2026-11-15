"""Happy-path tests for release.py: `just release-scripts-test`."""

import tempfile
import unittest
from pathlib import Path

import release

FILES = {
    "Cargo.toml": '[workspace.package]\nedition = "2021"\nrust-version = "1.74"\nversion = "1.0.0-beta.4"\n',
    "crates/sas-lexer-py/Cargo.toml": '[package]\nname = "sas-lexer-py"\nrust-version = "1.85"\nversion = "1.0.0-beta.2"\n',
    "pyproject.toml": '[project]\nname = "sas-lexer"\nversion = "1.0.0-beta.2"\n',
    "src/sas_lexer/__init__.py": '__version__ = "1.0.0-beta.2"\n',
    "CHANGELOG-PY.md": (
        "# Changelog\n\n<!-- next-header -->\n\n## [Unreleased] - ReleaseDate\n\n"
        "### Added\n-\n\n### Fixed\n- A fix.\n\n"
        "## [1.0.0-beta.2] - 2025-01-05\n\n### Fixed\n- Old fix.\n\n<!-- next-url -->\n"
        "[Unreleased]: https://github.com/mishamsk/sas-lexer/compare/py-v1.0.0-beta.2...HEAD\n"
    ),
}


class Release(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        for rel, text in FILES.items():
            (self.root / rel).parent.mkdir(parents=True, exist_ok=True)
            (self.root / rel).write_text(text)

    def test_prep_then_tag_check(self):
        py = release.TRAINS["py"]
        self.assertEqual(release.prep(self.root, py, "1.0.0-beta.3", "2026-11-15"), "1.0.0-beta.3")
        self.assertEqual(release.check_tag(self.root, py, "py-v1.0.0-beta.3"), "1.0.0-beta.3")
        changelog = (self.root / "CHANGELOG-PY.md").read_text()
        self.assertIn(
            "### Security\n-\n\n## [1.0.0-beta.3] - 2026-11-15\n\n### Fixed\n- A fix.\n\n## [1.0.0-beta.2]",
            changelog,
        )
        self.assertIn(
            "[Unreleased]: https://github.com/mishamsk/sas-lexer/compare/py-v1.0.0-beta.3...HEAD\n"
            "[1.0.0-beta.3]: https://github.com/mishamsk/sas-lexer/compare/py-v1.0.0-beta.2...py-v1.0.0-beta.3\n",
            changelog,
        )
        self.assertEqual(release.check(self.root, release.TRAINS["rust"]), "1.0.0-beta.4")

    def test_dev_version_and_artifacts(self):
        self.assertEqual(release.set_dev(self.root, "42"), "1.0.0b2.dev42")
        self.assertIn('version = "1.0.0b2.dev42"', (self.root / "pyproject.toml").read_text())
        dist = self.root / "dist"
        dist.mkdir()
        for name in (
            "sas_lexer-1.0.0b2.dev42.tar.gz",
            "sas_lexer-1.0.0b2.dev42-cp311-cp311-win_amd64.whl",
        ):
            (dist / name).touch()
        self.assertEqual(len(release.check_artifacts("1.0.0b2.dev42", dist)), 2)


if __name__ == "__main__":
    unittest.main()
