# Cleanup TODO

Follow-ups noted on 2026-10-13, after the Climate-Adjusted Provenancing Tool was unpublished.

- [ ] **Remove Shiny Server's example apps from the image.** `rocker/shiny` ships demos in `/srv/shiny-server` (`01_hello`, `sample-apps`, ...), and they are publicly reachable, e.g. `https://www.climatesmartnativeplants.org/01_hello/`. Add `RUN rm -rf /srv/shiny-server/*` before the app `COPY` lines in the `Dockerfile`.
- [ ] **Decide whether provenancing is coming back.** If not, delete `shiny-apps/provenancing/`, the commented-out lines in `Dockerfile` and `nginx/conf.d/default.conf`, and its README mentions. Then trim the packages only it needed (next item).
- [ ] **Trim unused R packages and system libraries.** `plant-selection` only loads shiny, dplyr, stringr, shinyWidgets, DT, shinyjs and data.table. `terra`, `geodata`, `gifski`, `AOI` and `climateR` aren't loaded by any app, and neither is the Rust install. `sf`, `leaflet`, `leafpop`, `duckdb` and `DBI` were only used by provenancing. Removing them would shrink the image and speed up builds.
- [ ] **Drop `dependencies=TRUE`.** It also installs every *suggested* package (about 200 extra). Listing the packages the apps actually load is enough.
- [ ] **Clean up the `sf` install line.** `configure.args` only applies to source builds, and sf now installs as a binary, so the flag does nothing.
- [ ] **Fix `nginx/html/contact.html`.** It links to `/about.html`, which doesn't exist, and it hardcodes its own header instead of loading `partials/header.html`.
- [ ] **Remove the stray `shiny-apps/provenancing/data/data/` folder.** It holds a copy of plant-selection CSVs that the app never reads. It's local only, since data folders are git-ignored.
- [ ] **Fill in the README deploy placeholders.** The ECR repository is `csnap-app` (us-east-2), and the ECS service is `csnap-service-sorigqkc` in cluster `default`.
- [ ] **Add an ECR lifecycle policy.** The repo has none, so every push leaves the previous ~6 GB image behind untagged. Keeping the last 3-5 images would preserve rollback while capping storage.
