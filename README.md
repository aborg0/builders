# Builders

This repository contains the builders projects and documentation. The CI builds across multiple JDKs and Scala versions and publishes artifacts to GitHub Packages.

Docs
-----

Check [builder_docs/target/mdoc/readme.md](builders_docs/target/mdoc/readme.md), in case it does not exist, generate it with:

```sbt
docs/mdoc
```

You can check the non-evaluated documentation at [docs/readme.md](docs/readme.md) as well.

CI and publishing
------------------

The GitHub Actions workflow runs on pushes to `main` and on tag pushes. It performs:
- Matrix build across JDKs 17, 21 and 25
- Cross-build across supported Scala versions (runs `sbt +test`)
- Publishes to GitHub Packages only once (on `main` or tag) to avoid duplicate publishes

Local quick commands
---------------------

Run all tests locally for all supported Scala versions:

```powershell
cd C:\Users\Gabor_Bakos\IdeaProjects\builders
sbt +test
```

Run tests for the local default Scala version:

```powershell
sbt test
```

Publish locally to your local ivy/maven repository (safe):

```powershell
sbt publishLocal
```

Publish to GitHub Packages (be careful — this will upload artifacts):

```powershell
# set env vars in PowerShell
$env:GITHUB_REPOSITORY = 'owner/repo'    # replace owner/repo
$env:GITHUB_ACTOR = 'your-username'      # the username to authenticate as
$env:GITHUB_TOKEN = 'ghp_...'            # use a PAT with package write scope or rely on Actions' GITHUB_TOKEN
sbt publish
```

Notes
------
- The `docs` subproject is excluded from publishing (it is only for documentation generation).
- If you want to restrict publishing to tags only (recommended for releases), update `.github/workflows/ci.yml`.
- If JDK 25 causes toolchain issues, remove it from the matrix until tooling stabilizes.
