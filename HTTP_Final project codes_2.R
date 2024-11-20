setwd("C:/Users/kawsa/OneDrive/Documents/R codes")
getwd()

library(httr)

#task1

get_wiki_covid19_page<-function(){
  
  wiki_base_url<-"https://en.wikipedia.org/w/index.php"
  query<-list(title="Template:COVID-19_testing_by_country")
  response<-GET(wiki_base_url,query=query)
  print(response)
}

get_wiki_covid19_page()


#task 2

# Load the required packages
library(rvest)

# Make the GET request
response <- GET("https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country")

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the HTML content
  page_content <- content(response, "text")
  html <- read_html(page_content)
  
  # Extract the second table on the page
  data_frame <- html %>%
    html_nodes("table") %>%  # Get all tables
    .[[2]] %>%                # Select the second table
    html_table(fill = TRUE)   # Convert the HTML table to a data frame
  
  # View the extracted data
  print(data_frame)
} else {
  print(paste("Error:", status_code(response)))
}

summary(data_frame)

colnames(data_frame)

#task 3

preprocess_covid_data_frame <- function(data_frame1) {
  
  shape <- dim(data_frame)
  
  # Remove the World row
  data_frame<-data_frame[!(data_frame$`Country.or.region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units.b."] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}


# Example of applying the function to the extracted data frame
# Assuming 'covid_table' is the data frame extracted from the Wikipedia page
data_frame1 <- preprocess_covid_data_frame(data_frame)


# View the preprocessed data
print(data_frame1)

print(names(data_frame1))

summary(data_frame1)

write.csv(preprocessed_data, file="C:/Users/kawsa/OneDrive/Documents/R codes/preprocessed_data.csv")
          

#task 4

## Download a sample csv file
covid_csv_file <- download.file("https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0101EN-Coursera/v2/dataset/covid.csv", destfile="covid.csv")
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")

str(covid_data_frame_csv)

covid_data_frame_csv[5:10,c("country", "confirmed")]


#task 5


total_confirmed <- sum(covid_data_frame_csv$confirmed,na.rm = TRUE)

total_tested <- sum(covid_data_frame_csv$tested, na.rm = TRUE)


cat("Total Confirmed Cases Worldwide:", total_confirmed, "\n")
cat("Total Tested Cases Worldwide:", total_tested, "\n")

positive_ratio<-total_confirmed/total_tested

cat("Overall positive ratio:", positive_ratio,"\n")


#task 6


covid_data_frame_csv$country

class(covid_data_frame_csv$country)

country_sorted<-sort(covid_data_frame_csv$country)

country_sorted

country_sorted2<-sort(covid_data_frame_csv$country, decreasing=TRUE)

country_sorted2



#task 7


grep("United.*",covid_data_frame_csv$country)

covid_data_frame_csv[164:166,"country"]


#task 8

str(covid_data_frame_csv)


covid_data_frame_csv[165:166,c("country","confirmed","confirmed.population.ratio")]


#task 9


if (8.9>6.3){
  print("United States of America rate is higher")
}else{
  print("United Kingdom rate is higher")
  }
    
#task 10

risk<-"low"

if (risk=="low"){
   subset(covid_data_frame_csv,confirmed.population.ratio<1)
} else {
  subset(covid_data_frame_csv,confirmed.population.ratio>=1)
}












