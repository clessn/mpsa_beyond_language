The python script in this repo should do the following stuff but so far we're stuck at step 3.

1. Go to the Eureka link 
2. wait for the user to connect 
3. Wait for the user to input the search query, the sources and the date period
4. Wait Click on the search button
5. Wait for the search results to load
6. Looking at the HTML generated and looking for the _docKeyList JS variable
7. Parse that variable to extract all the unique items
8. Use the extraction pattern found (i will share code with you) to generate the list of URLs of individual articles
9. Open a new tab
10. Search link 1
11. Extract the HTML and save it in a file: article_1.html
12. repeat for each articles

Here is the R code that transforms the _docKeyList JS variables into usable URLs:

pattern <- "_docKeyList = (\\[.+?\\]);"
match <- str_match(js_content, pattern)

# The second element of the match will contain the JSON array
json_array <- match[2]

# Parse the JSON array to get the document keys
doc_keys <- fromJSON(json_array)

# Show how many document keys we found
cat("Found", length(doc_keys), "document keys\n")

# Function to properly encode a document key for use in a URL
encode_doc_key <- function(doc_key) {
  # First, replace the special characters with their URL-encoded equivalents
  # This is done manually to ensure consistent encoding
  encoded <- doc_key
  encoded <- gsub("·", "%C2%B7", encoded)
  encoded <- gsub("×", "%C3%97", encoded)
  
  # Return the encoded string
  return(encoded)
}

# Create URLs for all document keys
urls_df <- data.frame(
  doc_key = doc_keys,
  stringsAsFactors = FALSE
)

# Add index starting from 0
urls_df$index <- 0:(nrow(urls_df) - 1)

# Create URLs with proper encoding
urls_df$url <- sapply(urls_df$doc_key, function(key) {
  encoded_key <- encode_doc_key(key)
  paste0(
    "https://nouveau-eureka-cc.acces.bibl.ulaval.ca/Document/View?viewEvent=1&docRefId=0&docName=",
    encoded_key,
    "&docIndex=0" # We'll adjust this later
  )
})

# Now update the docIndex parameter in each URL to match the index
for (i in 1:nrow(urls_df)) {
  urls_df$url[i] <- gsub("docIndex=0", paste0("docIndex=", urls_df$index[i]), urls_df$url[i])
}



Here is a truncated version of the <script> tag that contains the _docKeyList variable:

<script>
	    _MVCPath = "/Result/";
	    var _docKeyPos =  -1;
	    var _docKeyList = ["news·20241226·LM·202412262×20×22578961160×215","news·20241218·LM·202412182×20×22531266041","news·20241218·LM·202412182×20×22533440554","news·20241216·LM·202412162×20×22527506523","news·20241210·LM·202412102×20×22516479064","news·20241204·LF·1092×20×2698515687","news·20241202·LM·202412022×20×22482178862","news·20130907·LI·0dd2e468-1709-11e3-b57e-a58153285fc0"];
	    var _maxDoc = 999;
	    var _lblDate = "Date";
	    var _lblDomain = "Domaine couvert";
	    var _lblAdvCriteria = "Critère avancé";
		var _lblAdvCriteriaGrp = "Critères avancés";
        var txt_title_send = 'Envoi de références';
        var btn_Cancel = 'Annuler';
        var lbl_Send = 'Envoyer';
	    var _nrDoc = 999;
		var _codeFact = "";
		var _isInBasket = {"$type":"System.Collections.Generic.Dictionary`2[[System.String, mscorlib],[System.Boolean, mscorlib]], mscorlib","news·20241226·LM·202412262×20×22578961160×215":true,"news·20241218·LM·202412182×20×22531266041":true,"news·20241218·LM·202412182×20×22533440554":true,"news·20241216·LM·202412162×20×22527506523":true,"news·20241210·LM·202412102×20×22516479064":true,"news·20241204·LF·1092×20×2698515687":true,"news·20241202·LM·202412022×20×22482178862":true,"news·20241202·LM·202412022×20×22491257184":true,"news·20241116·LF·1092×20×2388058604":true,"news·20241114·LF·1092×20×2353402544":true,"news·20241109·LF·1092×20×2269199906":true,"news·20241104·LI·202411040895968830":true,"news·20241030·LF·1082×20×21855612504":true,"news·20241030·LF·1082×20×21855425371":true,"news·20241029·LI·202410290895932928":true,"news·20241026·LE·l00012331273":true,"news·20241022·LF·1082×20×21918517116":true,"news·20241019·OP·3d6f6d60-8de3-11ef-81f6-bb78ec196f4d":true,"news·20241018·LM·202410182×20×22353927630":true,"news·20241005·LI·202410050895994230":true,"news·20241004·LM·202410042×20×22317580284":true,"news·20241002·LF·1082×20×21560660407":true,"news·20240930·LM·202409302×20×22297228055":true,"news·20240927·LF·1082×20×21475816454":true,"news·20240925·LM·202409252×20×22282488856":true,"news·20240925·LM·202409252×20×22282516431":true,"news·20240925·LM·202409252×20×22282486913":true,"news·20240923·LM·202409232×20×22285594318":true,"news·20240918·LF·1082×20×21268083068":true,"news·20240914·LE·l00012324459":true,"news·20240913·LI·202409135015389662":true,"news·20240912·LF·1082×20×21198836346":true,"news·20240912·LM·202409122×20×22263272954":true,"news·20240911·LM·202409112×20×22253206677":true,"news·20240909·LM·202409092×20×22253078196":true,"news·20240904·LF·1082×20×21082380712":true,"news·20240902·LM·202409022×20×22236974252":true,"news·20240820·LM·202408202×20×22210724435":true,"news·20240724·LM·202407242×20×22169010839":true,"news·20240720·LI·202407205015376918":true,"news·20240713·LM·202407132×20×22154432345":true,"news·20240710·LM·202407102×20×22145626601":true,"news·20240705·LM·202407052×20×22143574470":true,"news·20240620·LM·202406202×20×22122346322":true,"news·20240620·LM·202406202×20×22122347610":true,"news·20240619·LF·1072×20×21662130924":true,"news·20240613·LM·202406132×20×22114050820":true,"news·20240610·LM·202406102×20×22107551303":true,"news·20240610·LM·202406102×20×22107521944":true,"news·20240604·LE·l000012315720":true};
		var lbl_noData = '<div class="noDataNews"><img src="/images/interface/search/no_data_available_fr.svg"/></div>';
		var _toolType = 1;
		var PreferencesPresentation = 1;
	</script>
