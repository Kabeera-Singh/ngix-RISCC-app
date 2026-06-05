# Climate-Smart Native Plants Tool (C-SNaP Tool)

Interactive web tools for climate-smart plant selection and seed provenancing across U.S. states.

**Live site:** [https://www.climatesmartnativeplants.org](https://www.climatesmartnativeplants.org)

## What This Project Contains

| Component | Description |
|-----------|-------------|
| **plant-selection** | Filter and rank native plants by state, hardiness zone, growth habit, and climate resilience |
| **provenancing** | Find seed sources matched to your site's climate using lookup tables |
| **Homepage + pages** | Static landing page, contact page, and resources page |

## How It Works

```
Browser request
     │
     ▼
┌──────────┐        ┌──────────────┐
│  Nginx   │───────▶│ Shiny Server │
│ (port 80)│        │ (port 3838)  │
└──────────┘        └──────────────┘
     │                     │
     │ static HTML/CSS     │ R Shiny apps
     ▼                     ▼
 nginx/html/        shiny-apps/
  ├── index.html      ├── plant-selection/
  ├── contact.html     └── provenancing/
  └── resources.html
```

Everything runs inside a single Docker container. **Nginx** serves the static pages and reverse-proxies `/plant-selection/` and `/provenancing/` to **Shiny Server** on port 3838. **Supervisor** keeps both processes running.

## Prerequisites

Install these before you start:

| Tool | Purpose | Install |
|------|---------|---------|
| **Docker Desktop** | Build and run the container | [docker.com/products/docker-desktop](https://www.docker.com/products/docker-desktop) |
| **Git** | Clone the repo and push changes | [git-scm.com](https://git-scm.com/) |
| **R** (optional) | Run data preprocessing locally | [r-project.org](https://www.r-project.org/) |
| **AWS CLI** | Push images to ECR and manage deployments | [aws.amazon.com/cli](https://aws.amazon.com/cli/) |

You will also need:

- A **GitHub** account with access to this repository
- A **Docker Hub** account (free tier is fine)
- An **AWS account** with permissions for ECR (Elastic Container Registry) and ECS (Elastic Container Service)

## First-Time Setup (do this once)

### 1. Configure Git

Tell Git who you are (used in commit messages):

```bash
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
```

### 2. Log in to Docker Hub

Docker Hub login is required so you can pull base images without hitting rate limits:

```bash
docker login
```

Enter your Docker Hub username and password (or a personal access token) when prompted. You stay logged in until you explicitly log out.

### 3. Configure the AWS CLI

Run the interactive setup and enter your AWS credentials:

```bash
aws configure
```

It will ask for four values:

| Prompt | What to enter |
|--------|---------------|
| AWS Access Key ID | Your IAM access key (starts with `AKIA...`) |
| AWS Secret Access Key | The secret that pairs with the access key |
| Default region name | `us-east-2` |
| Default output format | `json` |

Get your access key from the [AWS IAM Console](https://console.aws.amazon.com/iam/) under **Users > Your User > Security credentials > Access keys**. If you don't have one, ask the project admin to create an IAM user for you with ECR and ECS permissions.

### 4. Log in to AWS ECR

This authenticates Docker with your private ECR registry so you can push images:

```bash
aws ecr get-login-password --region us-east-2 | \
  docker login --username AWS --password-stdin <ACCOUNT_ID>.dkr.ecr.us-east-2.amazonaws.com
```

Replace `<ACCOUNT_ID>` with the 12-digit AWS account number. This login expires after 12 hours -- you will need to re-run it before each push session.

## Local Development

### 1. Clone the repository

```bash
git clone https://github.com/Kabeera-Singh/ngix-RISCC-app.git
cd ngix-RISCC-app
```


### 1.1 Update the respository

If other people have made changes to the repository since you cloned it, pull the latest changes before you start working:

```bash
git pull
```

### 2. Get the data files

**Important:** This repo's `.gitignore` contains `*data*`, which means data files and data directories are excluded from Git. You must obtain them separately.

Download the data files from the shared Google Drive folder and place them in:

- `shiny-apps/plant-selection/data/` — needs `ClimateSmart_Data_Cleaned.csv` (and any other CSVs the app references)
- `shiny-apps/provenancing/data/` — needs `climate_lookup_table.csv`

### 3. Preprocess the plant-selection data (optional)

This converts the CSV into compact RDS files so the plant-selection app starts faster and uses less RAM:

```bash
Rscript shiny-apps/plant-selection/prepare_plant_info.R
```

The script produces:
- `data/plants_prepared.rds` — full cleaned dataset
- `data/plants_by_state/*.rds` — one file per state for fast filtering
- `data/filter_options.rds` — pre-computed filter/sorting options

If you skip this step, the app will fall back to loading and processing the CSV at startup (slower but still works).

### 4. Build and run locally

```bash
docker build -t csnap-app .
docker run -p 80:80 csnap-app
```

Open [http://localhost](http://localhost) in your browser. You should see the homepage with links to both tools:
- [http://localhost/plant-selection/](http://localhost/plant-selection/)
- [http://localhost/provenancing/](http://localhost/provenancing/)

Press `Ctrl+C` to stop the container.

**Note:** The first Docker build takes a long time (20-40 minutes) because it installs spatial R packages (`sf`, `terra`, `geodata`) and packages from GitHub (`AOI`, `climateR`). Subsequent builds use cached layers and are much faster unless you change the package-install lines in the Dockerfile.

## Making Changes

### Edit an existing Shiny app

App code lives in `shiny-apps/<app-name>/app.R`. Edit the file, rebuild Docker, and test locally:

```bash
docker build -t csnap-app .
docker run -p 80:80 csnap-app
```

### Edit the homepage or static pages

- Homepage: `nginx/html/index.html`
- Contact page: `nginx/html/contact.html`
- Resources page: `nginx/html/resources.html`
- Shared header/footer partials: `nginx/html/partials/`
- CSS: `nginx/html/www/css/`
- Shared color tokens: `nginx/html/www/css/shared-colors.css` (also copied into both Shiny apps by the Dockerfile)

### Add a new Shiny app

1. Create a folder: `shiny-apps/my-new-app/` with an `app.R` file
2. Add a COPY line to the **Dockerfile**:
   ```dockerfile
   COPY shiny-apps/my-new-app/ /srv/shiny-server/my-new-app/
   ```
3. Add a location block to **nginx/conf.d/default.conf** (copy an existing one and change the name):
   ```nginx
   location /my-new-app/ {
       proxy_pass http://shiny-server/my-new-app/;
       proxy_http_version 1.1;
       proxy_set_header Upgrade $http_upgrade;
       proxy_set_header Connection $connection_upgrade;
       proxy_set_header Host $host;
       proxy_set_header X-Real-IP $remote_addr;
       proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
       proxy_set_header X-Forwarded-Proto $scheme;
       proxy_cache_bypass $http_upgrade;
       proxy_read_timeout 86400;
       proxy_redirect off;
   }
   ```
4. Add a link on the homepage in **nginx/html/index.html**
5. If the app uses new R packages, add them to the `install.packages()` lines in the **Dockerfile**

### Add R packages

Find the `install.packages(...)` lines in the Dockerfile and add your package name to the list:

```dockerfile
RUN R -e "install.packages(c('shiny', 'tidyverse', ..., 'YOUR_PACKAGE'), repos='https://cran.rstudio.com/', dependencies=TRUE)"
```

For packages only available on GitHub, add a new line:

```dockerfile
RUN R -e "remotes::install_github('author/package')"
```

## Pushing Changes to GitHub

Pushing to GitHub saves your code but does **not** update the live site. You must also deploy to AWS (next section).

```bash
git add .
git commit -m "describe what you changed"
git push
```

If someone else pushed changes before you, pull first:

```bash
git pull --rebase
git push
```

**Watch out for the `.gitignore`:** Because `*data*` is in `.gitignore`, Git will ignore any file or folder with "data" in its name. This is intentional for large data files, but if you create a non-data file with "data" in the name, you will need to force-add it with `git add -f <filename>`.

## Deploying to AWS

The live site runs on **AWS ECS Fargate** in the **us-east-2** region. The deployment flow is:

```
Build Docker image ──▶ Push to ECR ──▶ Force new ECS deployment
```

### Step 1: Build the image for AWS

AWS runs on Linux/AMD64, so even on a Mac with Apple Silicon you need to specify the platform:

```bash
docker build -t csnap-app .
```

### Step 2: Log in to AWS ECR

```bash
aws ecr get-login-password --region us-east-2 | \
  docker login --username AWS --password-stdin <ACCOUNT_ID>.dkr.ecr.us-east-2.amazonaws.com
```

Replace `<ACCOUNT_ID>` with your AWS account number (12 digits).

### Step 3: Tag and push the image

```bash
docker tag csnap-app:latest <ACCOUNT_ID>.dkr.ecr.us-east-2.amazonaws.com/<ECR_REPO>:latest
docker push <ACCOUNT_ID>.dkr.ecr.us-east-2.amazonaws.com/<ECR_REPO>:latest
```

Replace `<ECR_REPO>` with the ECR repository name for C-SNaP.

### Step 4: Deploy the new image on ECS

1. Go to the [AWS ECS Console](https://us-east-2.console.aws.amazon.com/ecs/v2/clusters?region=us-east-2)
2. Select the cluster and service running C-SNaP
3. Click **Update service**
4. Check **Force new deployment**
5. Click **Update**

ECS will pull the latest image from ECR and replace the running container. This typically takes 2-5 minutes.

### Step 5: Verify the deployment

```bash
curl -sk -o /dev/null -w "%{http_code}" https://www.climatesmartnativeplants.org/
```

You should see `200`. Open the site in your browser to confirm the changes are live.

## Custom Domain and SSL

The domain `www.climatesmartnativeplants.org` is managed through:

- **AWS ALB** (Application Load Balancer) — receives HTTPS traffic and forwards to ECS
- **AWS ACM** (Certificate Manager) — provides the SSL certificate for `climatesmartnativeplants.org`
- **Hover** (domain registrar) — DNS records point `www` to the ALB via a CNAME record

You generally do not need to change DNS or SSL settings unless you are adding a new domain.

## Project Structure

```
ngix-RISCC-app/
├── Dockerfile                  # Builds the container (Nginx + Shiny + R packages)
├── README.MD                   # This file
├── .gitignore                  # Excludes *data* paths from Git
├── render.yaml                 # Legacy Render deployment config (not used)
├── supervisor.conf             # Runs Nginx and Shiny Server together
├── shiny-server.conf           # Shiny Server settings (port 3838)
├── shiny-apps/
│   ├── plant-selection/
│   │   ├── app.R               # Shiny app code
│   │   ├── prepare_plant_info.R # CSV → RDS preprocessing script
│   │   ├── www/                # App-specific CSS
│   │   └── data/               # CSVs and RDS files (not in Git)
│   └── provenancing/
│       ├── app.R
│       ├── www/
│       └── data/
│           └── climate_lookup_table.csv
└── nginx/
    ├── conf.d/default.conf     # Nginx routing rules
    └── html/
        ├── index.html          # Homepage
        ├── contact.html        # Contact page
        ├── resources.html      # Resources page
        ├── partials/           # Shared header/footer HTML
        └── www/css/            # Stylesheets (including shared-colors.css)
```

## Troubleshooting

### Apps show a grey overlay or "Disconnected from server"

This is usually caused by **Zscaler** or another corporate SSL inspection proxy intercepting WebSocket connections. The apps are configured with `disable_websockets;` in `shiny-server.conf` to fall back to XHR long-polling, which works around most proxy issues. If the overlay still appears:
- Try on a personal network (not behind a corporate proxy)
- Try on a phone or a machine without Zscaler

### `docker build` fails on Apple Silicon Mac

Make sure you include `--platform linux/amd64`:

```bash
docker build --platform linux/amd64 -t csnap-app .
```

### `docker build` fails installing spatial R packages

The `sf`, `terra`, `geodata`, `AOI`, and `climateR` packages require system-level spatial libraries (GDAL, PROJ, GEOS) and Rust (for `gifski`). These are installed by the Dockerfile automatically, but builds can fail if upstream package versions change. If this happens, check the error output for the specific package and version, and look for updated install instructions on the package's GitHub page.

### ECS shows "503 Service Temporarily Unavailable"

- Check that the ECS service has healthy targets in its target group
- Verify the target group uses **IP** target type on port **80**
- Make sure the ALB health check path is `/` and the target returns HTTP 200

### I pushed to GitHub but the live site didn't change

Pushing to GitHub only saves your code. You must also build, push to ECR, and force a new ECS deployment (see the Deploying to AWS section).

### Data files are missing or Git won't track them

- The `data/` directories are excluded by `.gitignore` (`*data*`). Download the data files from the shared Google Drive folder.
- If you need Git to track a file that has "data" in its name, use `git add -f <filename>`.

## Port Reference

| Service | Port | Visibility |
|---------|------|------------|
| Nginx | 80 | External (exposed to browser/ALB) |
| Shiny Server | 3838 | Internal only (Nginx proxies to it) |
