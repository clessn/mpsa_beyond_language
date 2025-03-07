library(RSelenium)
library(wdman)

# Start the Selenium Server
selenium_server <- selenium(
  retcommand = TRUE,
  check = TRUE
)

# Print the command to start the server (optional)
print(selenium_server)

# Connect to the Selenium Server
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "chrome"
)

# Open the browser
remDr$open()
