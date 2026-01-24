# Script to deploy the Shiny app to shinyapps.io
# Run this script in R or RStudio

# Install rsconnect if not already installed
if (!require("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

library(rsconnect)

# Set your account info (you'll need to get these from shinyapps.io)
# To get your token and secret:
# 1. Go to https://www.shinyapps.io/
# 2. Log in to your account
# 3. Click on your account name -> Tokens
# 4. Click "Add Token" and copy the token and secret

# Option 1: If you haven't connected before, use this:
# rsconnect::setAccountInfo(
#   name = "your-account-name",
#   token = "your-token",
#   secret = "your-secret"
# )

# Option 2: If already connected, just deploy:
rsconnect::deployApp(
  appDir = ".",
  appName = "ecole-de-simulateur-prix",
  appTitle = "Simulateur de prix - École Pierre et Marie Curie 2026-2027",
  account = "ecole",
  server = "shinyapps.io"
)
