import json
import os
import platform
import re
import select
import sys
import time

import pandas as pd
import undetected_chromedriver as uc
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from webdriver_manager.chrome import ChromeDriverManager


class EurekaScraper:
    def __init__(self, output_dir="./downloaded_articles"):
        """Initialize the Eureka scraper with configuration."""
        self.output_dir = output_dir
        self.driver = None

        # Create output directory if it doesn't exist
        if not os.path.exists(output_dir):
            os.makedirs(output_dir)

    def setup_driver(self):
        """Set up the Selenium WebDriver using undetected-chromedriver."""
        print("Setting up undetected Chrome WebDriver...")
        try:
            # Configure Chrome options
            chrome_options = uc.ChromeOptions()

            # Add options for stability
            chrome_options.add_argument("--no-sandbox")
            chrome_options.add_argument("--disable-dev-shm-usage")
            chrome_options.add_argument("--window-size=1920,1080")

            # Create the undetected-chromedriver
            self.driver = uc.Chrome(options=chrome_options)

            # Set implicit wait time
            self.driver.implicitly_wait(10)
            print("Undetected Chrome browser set up successfully")
        except Exception as e:
            print(f"Error setting up undetected Chrome: {e}")
            print("Falling back to regular Chrome WebDriver...")

            # Fallback to regular Chrome WebDriver
            chrome_options = Options()
            chrome_options.add_argument("--no-sandbox")
            chrome_options.add_argument("--disable-dev-shm-usage")
            chrome_options.add_argument("--window-size=1920,1080")
            chrome_options.add_argument(
                "user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36"
            )

            self.driver = webdriver.Chrome(
                service=Service(ChromeDriverManager().install()), options=chrome_options
            )
            self.driver.implicitly_wait(10)
            print("Fallback Chrome browser set up successfully")

    def navigate_to_initial_url(self, url):
        """Navigate to the initial URL."""
        print(f"Navigating to: {url}")
        self.driver.get(url)
        self.driver.save_screenshot(os.path.join(self.output_dir, "initial_page.png"))

    def manual_login(self):
        """Allow the user to manually log in."""
        print("\n" + "=" * 50)
        print("MANUAL LOGIN REQUIRED")
        print("=" * 50)
        print("Please log in manually in the opened browser window.")
        print("Once you are ready to proceed, press Enter in this console.")
        print("=" * 50 + "\n")

        # Wait for user to press Enter
        input("Press Enter to continue after you've completed the search setup...")

        # Save current page to help with debugging
        self.driver.save_screenshot(
            os.path.join(self.output_dir, "after_manual_login.png")
        )

    def wait_for_search_results(self):
        """Wait for the search results to be visible after user manually searches."""
        print("Waiting for search results page...")

        # Take a screenshot to document the current state
        self.driver.save_screenshot(os.path.join(self.output_dir, "search_results.png"))

        # No need to click search button or set up search parameters
        # User has done it manually, we just need to extract the results
        print("Search results page loaded")

    def extract_doc_keys(self):
        """Extract the _docKeyList JavaScript variable and parse it."""
        try:
            # Take a screenshot of the results page
            self.driver.save_screenshot(
                os.path.join(self.output_dir, "results_page.png")
            )

            # Try to find the number of results first
            try:
                results_count = self.driver.find_element(
                    By.XPATH, "//div[contains(@class, 'results-count')]"
                )
                print(f"Results indicator found: {results_count.text}")
            except Exception as e:
                print(f"Could not find results count: {e}")

            # Get page source
            page_source = self.driver.page_source

            # Save page source for debugging
            with open(
                os.path.join(self.output_dir, "results_page_source.html"),
                "w",
                encoding="utf-8",
            ) as f:
                f.write(page_source)

            # Try different regex patterns to find document keys
            patterns = [
                r"_docKeyList\s*=\s*(\[.+?\]);",  # Original pattern
                r"var\s+_docKeyList\s*=\s*(\[.+?\]);",  # With 'var' declaration
                r"_docKeyList\s*=\s*(\[.*?\])",  # Without semicolon
                r"docKeyList\s*=\s*(\[.+?\]);",  # Alternative variable name
                r"\"docKeys\":\s*(\[.+?\])",  # JSON property pattern
            ]

            doc_keys = []

            # Try each pattern until we find a match
            for pattern in patterns:
                match = re.search(pattern, page_source, re.DOTALL)
                if match:
                    json_array = match.group(1)

                    # Clean the JSON string (handle escaped quotes, newlines, etc.)
                    json_array = json_array.replace('\\"', '"').replace("\\n", "")

                    try:
                        doc_keys = json.loads(json_array)
                        print(
                            f"Found {len(doc_keys)} document keys with pattern: {pattern}"
                        )
                        break
                    except json.JSONDecodeError as e:
                        print(f"Error parsing JSON array with pattern {pattern}: {e}")
                        continue

            # Alternative: Try to extract document IDs from links if regex approach fails
            if not doc_keys:
                print("Trying alternative extraction method from links...")
                try:
                    article_links = self.driver.find_elements(
                        By.XPATH, "//a[contains(@href, 'docName=')]"
                    )
                    for link in article_links:
                        href = link.get_attribute("href")
                        match = re.search(r"docName=([^&]+)", href)
                        if match:
                            doc_key = match.group(1)
                            # URL decode the key
                            doc_key = doc_key.replace("%C2%B7", "·").replace(
                                "%C3%97", "×"
                            )
                            if doc_key not in doc_keys:
                                doc_keys.append(doc_key)

                    print(f"Found {len(doc_keys)} document keys from links")
                except Exception as e:
                    print(f"Error extracting keys from links: {e}")

            # Save doc keys to file for backup (if any found)
            if doc_keys:
                with open(
                    os.path.join(self.output_dir, "doc_keys.json"),
                    "w",
                    encoding="utf-8",
                ) as f:
                    json.dump(doc_keys, f, indent=2)

                return doc_keys
            else:
                print("Could not find any document keys using available methods")
                return []

        except Exception as e:
            print(f"Error extracting document keys: {e}")
            self.driver.save_screenshot(
                os.path.join(self.output_dir, "doc_keys_error.png")
            )
            return []

    def encode_doc_key(self, doc_key):
        """Properly encode a document key for use in a URL."""
        encoded = doc_key
        encoded = encoded.replace("·", "%C2%B7")
        encoded = encoded.replace("×", "%C3%97")
        return encoded

    def create_article_urls(self, doc_keys):
        """Create URLs for all document keys."""
        urls = []

        for index, key in enumerate(doc_keys):
            encoded_key = self.encode_doc_key(key)
            url = f"https://nouveau-eureka-cc.acces.bibl.ulaval.ca/Document/View?viewEvent=1&docRefId=0&docName={encoded_key}&docIndex={index}"
            urls.append({"doc_key": key, "index": index, "url": url})

        print(f"Created {len(urls)} article URLs")

        # Save URLs to CSV file
        urls_df = pd.DataFrame(urls)
        urls_df.to_csv(os.path.join(self.output_dir, "article_urls.csv"), index=False)

        return urls

    def download_articles(self, urls, start_index=0, max_articles=None):
        """Download articles from the generated URLs."""
        if not urls:
            print("No URLs to download. Exiting.")
            return

        if max_articles is None:
            max_articles = len(urls)

        end_index = min(start_index + max_articles, len(urls))

        # Check for existing articles to enable resuming downloads
        existing_articles = []
        for i in range(start_index, end_index):
            article_index = urls[i]["index"]
            filename = os.path.join(
                self.output_dir, f"article_{article_index+1:04d}.html"
            )
            if os.path.exists(filename):
                existing_articles.append(article_index)

        if existing_articles:
            print(
                f"Found {len(existing_articles)} existing articles that will be skipped"
            )

        for i in range(start_index, end_index):
            url_data = urls[i]
            article_url = url_data["url"]
            article_index = url_data["index"]

            # Skip existing articles
            filename = os.path.join(
                self.output_dir, f"article_{article_index+1:04d}.html"
            )
            if article_index in existing_articles:
                print(
                    f"Skipping article {i+1-start_index}/{end_index-start_index} (index {article_index}) - already downloaded"
                )
                continue

            try:
                print(
                    f"Downloading article {i+1-start_index}/{end_index-start_index} (index {article_index})"
                )

                # Open article in the current window
                self.driver.get(article_url)

                # Reduced wait time for faster downloads
                time.sleep(1.5)

                # Get the HTML content
                article_html = self.driver.page_source

                # Save the HTML content to a file
                with open(filename, "w", encoding="utf-8") as f:
                    f.write(article_html)

                print(f"Saved article to {filename}")

                # Only take screenshots of the first 3 articles for verification
                if i < start_index + 3:
                    self.driver.save_screenshot(
                        os.path.join(
                            self.output_dir,
                            f"article_{article_index+1:04d}_screenshot.png",
                        )
                    )

                # Reduced delay between requests for faster processing
                time.sleep(1)

                # Save progress more frequently (every 10 articles)
                if (i - start_index + 1) % 10 == 0:
                    with open(os.path.join(self.output_dir, "progress.txt"), "w") as f:
                        f.write(f"Current progress: {i+1}/{end_index}\n")
                        f.write(f"Timestamp: {time.strftime('%Y-%m-%d %H:%M:%S')}\n")

            except Exception as e:
                print(f"Error downloading article {article_index+1:04d}: {e}")

                # Save error details
                error_file = os.path.join(
                    self.output_dir, f"article_{article_index+1:04d}_error.txt"
                )
                with open(error_file, "w", encoding="utf-8") as f:
                    f.write(f"Error: {str(e)}\n")
                    f.write(f"URL: {article_url}\n")

                # Give a short break after errors to prevent rapid failure
                time.sleep(2)

    def close(self):
        """Close the browser."""
        if self.driver:
            self.driver.quit()
            print("Browser closed")

    def run(self, start_url, start_index=0, batch_size=1000):
        """Run the complete scraping process with manual login and search."""
        try:
            # Setup the browser
            self.setup_driver()

            # Navigate to the starting URL
            self.navigate_to_initial_url(start_url)

            # Let the user manually login and perform search
            self.manual_login()

            # User has manually done the search, now we wait for the results
            self.wait_for_search_results()

            # Extract document keys
            doc_keys = self.extract_doc_keys()
            if not doc_keys:
                print("No document keys found. Exiting.")
                return

            # Create URLs for articles
            urls = self.create_article_urls(doc_keys)

            # Download articles in batches
            total_articles = len(urls)
            current_index = start_index

            print(f"Found {total_articles} articles to download")
            print("Will download ALL articles in the search results")

            # Ask user if they want to continue
            if total_articles > 100:
                response = input(
                    f"You are about to download {total_articles} articles. Continue? (y/n): "
                )
                if response.lower() != "y":
                    print("Download cancelled by user")
                    return

            while current_index < total_articles:
                batch_end = min(current_index + batch_size, total_articles)
                print(
                    f"\nProcessing batch: {current_index+1} to {batch_end} (of {total_articles})"
                )

                self.download_articles(urls, current_index, batch_size)

                current_index += batch_size

                # Save progress
                with open(os.path.join(self.output_dir, "progress.txt"), "w") as f:
                    f.write(f"Last processed index: {current_index}\n")
                    f.write(
                        f"Articles downloaded: {min(current_index, total_articles)}/{total_articles}\n"
                    )
                    f.write(f"Timestamp: {time.strftime('%Y-%m-%d %H:%M:%S')}\n")

                print(
                    f"Completed {min(current_index, total_articles)}/{total_articles} articles"
                )

                # If we have more batches to go, take a short break
                if current_index < total_articles:
                    pause_time = 5  # Reduced pause time between batches
                    print(f"Pausing for {pause_time} seconds before the next batch...")
                    time.sleep(pause_time)

            print("\nArticle download complete!")

        except Exception as e:
            print(f"Error in scraping process: {e}")
            if self.driver:
                self.driver.save_screenshot(
                    os.path.join(self.output_dir, "error_screenshot.png")
                )

        finally:
            # Handle script termination more gracefully
            try:
                print("\nKeeping browser open for inspection.")
                print(
                    "When you're done, close the browser manually and press Ctrl+C to exit the script."
                )

                # Give time for user to see the final state before closing
                time.sleep(10)

                # Close the browser if it's still open
                if self.driver:
                    self.close()

            except KeyboardInterrupt:
                print("\nScript interrupted by user. Closing browser...")
                if self.driver:
                    self.close()
            except Exception as e:
                print(f"\nError during cleanup: {e}")
                if self.driver:
                    self.close()


def validate_date_format(date_str):
    """Validate that a string is in YYYY-MM-DD format."""
    try:
        time.strptime(date_str, "%Y-%m-%d")
        return True
    except ValueError:
        return False


def main(
    output_dir="./eureka_articles",
    start_index=0,
    resume=False,
    start_date=None,
    end_date=None,
):
    """Main function to run the scraper with configurable parameters."""
    start_url = "https://acces.bibl.ulaval.ca/login?url=https://nouveau.eureka.cc/access/ip/default.aspx?un=ulaval1"

    # Ask for timeframe if not provided
    if not start_date:
        while True:
            start_date = input("Enter the start date (YYYY-MM-DD): ")
            if validate_date_format(start_date):
                break
            print("Invalid date format. Please use YYYY-MM-DD format.")

    if not end_date:
        while True:
            end_date = input("Enter the end date (YYYY-MM-DD): ")
            if validate_date_format(end_date):
                break
            print("Invalid date format. Please use YYYY-MM-DD format.")

    # Create a timeframe-specific directory
    timeframe_dir = os.path.join(output_dir, f"{start_date}_{end_date}")
    if not os.path.exists(timeframe_dir):
        os.makedirs(timeframe_dir)
        print(f"Created directory: {timeframe_dir}")

    # Check if we need to resume from a saved progress file
    if resume and os.path.exists(os.path.join(timeframe_dir, "progress.txt")):
        try:
            with open(os.path.join(timeframe_dir, "progress.txt"), "r") as f:
                for line in f:
                    if line.startswith("Last processed index:"):
                        resume_index = int(line.split(":")[1].strip())
                        start_index = resume_index
                        print(
                            f"Resuming from index {start_index} based on saved progress"
                        )
                        break
        except Exception as e:
            print(
                f"Error reading progress file, starting from index {start_index}: {e}"
            )

    # Create and run the scraper
    print("Starting Eureka article scraper")
    print(f"Output directory: {timeframe_dir}")
    print(f"Starting index: {start_index}")
    print(f"Timeframe: {start_date} to {end_date}")

    scraper = EurekaScraper(output_dir=timeframe_dir)
    scraper.run(start_url=start_url, start_index=start_index)


if __name__ == "__main__":
    import argparse
    import sys

    # Check if we're running in IPython
    try:
        __IPYTHON__
        in_ipython = True
    except NameError:
        in_ipython = False

    if in_ipython:
        # When in IPython, don't use argparse - just run with defaults
        print("Running in IPython environment with default settings")
        main()
    else:
        # Create command line argument parser for normal Python execution
        parser = argparse.ArgumentParser(description="Eureka Article Scraper")
        parser.add_argument(
            "--output-dir",
            type=str,
            default="./eureka_articles",
            help="Directory to save downloaded articles (default: ./eureka_articles)",
        )
        parser.add_argument(
            "--start-date",
            type=str,
            help="Start date for article search (YYYY-MM-DD format)",
        )
        parser.add_argument(
            "--end-date",
            type=str,
            help="End date for article search (YYYY-MM-DD format)",
        )
        parser.add_argument(
            "--start-index",
            type=int,
            default=0,
            help="Starting index for downloading (useful for resuming) (default: 0)",
        )
        parser.add_argument(
            "--resume", action="store_true", help="Resume from last saved progress"
        )

        args = parser.parse_args()

        # Run with command line arguments
        main(
            output_dir=args.output_dir,
            start_index=args.start_index,
            resume=args.resume,
            start_date=args.start_date,
            end_date=args.end_date,
        )
