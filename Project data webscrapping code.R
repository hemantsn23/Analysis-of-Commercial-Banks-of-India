# Install and load the required libraries
install.packages("rvest")
install.packages("dplyr")
library(rvest)
library(dplyr)

# setwd("C:/Users/dell/Desktop/IIT Kanpur/Data science lab/Project")
# As here setwd is given so in the process of csv making( which we have done in the later stage of this code), we aren't providing full path
# If setwd isn't given then path directory should be there to save the csv file


# Specify the URL of the Wikipedia page
url <- "https://en.wikipedia.org/wiki/List_of_banks_in_India"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Find the table containing private sector bank data
private_sector_banks <- webpage %>%
  html_nodes("table.wikitable") %>%
  .[[2]]

# Extract the table data
private_sector_banks <- private_sector_banks %>%
  html_table(fill = TRUE)


# Convert the list to a data frame
private_sector_banks <- as.data.frame(private_sector_banks)

# Remove the "Refs" column if it exists
if ("Refs" %in% colnames(private_sector_banks)) {
  private_sector_banks <- private_sector_banks[, -which(colnames(private_sector_banks) == "Refs")]
}
# cleaning data of private sector banks
y2 <- strsplit(private_sector_banks$'Total Assets'," ")
y3 <- sapply(y2, "[[",1)
y3 <- gsub("[^A-Za-z0-9]","",y3)
private_sector_banks$'Total Assets' <- y3
private_sector_banks

z2 <- strsplit(private_sector_banks$Revenues," ")
z3 <- sapply(z2, "[[",1)
z3 <- gsub("[^A-Za-z0-9]","",z3)
private_sector_banks$Revenues <- z3

# Print the extracted table
print(private_sector_banks)








# Public sector banks

url <- "https://en.wikipedia.org/wiki/List_of_banks_in_India"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Find the table containing public sector bank data
public_sector_banks <- webpage %>%
  html_nodes("table.wikitable") %>%
  .[[1]]  # Assuming the second "wikitable" is for public sector banks

# Extract the table data
public_sector_banks <- public_sector_banks %>%
  html_table(fill = TRUE)

# Convert the list to a data frame
public_sector_banks <- as.data.frame(public_sector_banks)
# removing the Refs column
if ("Refs" %in% colnames(public_sector_banks)) {
  public_sector_banks <- public_sector_banks[, -which(colnames(public_sector_banks) == "Refs")]
}
# cleaning data of public sector banks
y <- strsplit(public_sector_banks$'Total Assets'," ")
y1 <- sapply(y, "[[",1)
y1 <- gsub("[^A-Za-z0-9]","",y1)
public_sector_banks$'Total Assets' <- y1
public_sector_banks

z <- strsplit(public_sector_banks$Revenues," ")
z1 <- sapply(z, "[[",1)
z1 <- gsub("[^A-Za-z0-9]","",z1)
public_sector_banks$Revenues <- z1
z4 <- strsplit(public_sector_banks$'Government  Shareholding',"%") 
z4 <- sapply(z4, "[[", 1)
public_sector_banks$'Government  Shareholding' <- z4

# Print the extracted table for public sector banks
print(public_sector_banks)









# Regional rural banks

url <- "https://en.wikipedia.org/wiki/List_of_banks_in_India"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Find the table containing Regional Rural Bank (RRB) data
rrb_banks <- webpage %>%
  html_nodes("table.wikitable") %>%
  .[[3]]  # Assuming the third "wikitable" is for RRBs

# Extract the table data
rrb_banks <- rrb_banks %>%
  html_table(fill = TRUE)

# Convert the list to a data frame
rrb_banks <- as.data.frame(rrb_banks)

# Remove the "Refs" column 
if ("Refs" %in% colnames(rrb_banks)) {
  rrb_banks <- rrb_banks[, -which(colnames(rrb_banks) == "Refs")]
}
# remove the numerical values at the end the banks' name
rrb_banks <- rrb_banks %>% rename( bank_name = 'Bank\'s Name')


x<- strsplit(rrb_banks$bank_name, "[",fixed = TRUE)
sapply(x , "[[",1)
rrb_banks$bank_name <- sapply(x , "[[",1)

# Print the extracted table for RRBs
print(rrb_banks)

# Our group 15 final dataset containing three independent tables
print(rrb_banks)
print(private_sector_banks)
print(public_sector_banks)



# making the csv file
regional_rural_banks.csv<- rrb_banks
write.csv(rrb_banks,"regional_rural_banks.csv", row.names= FALSE)
private_banks.csv<- private_sector_banks
write.csv(private_sector_banks,"private_banks.csv",row.names = FALSE)
public_banks.csv<- public_sector_banks
write.csv(public_sector_banks,"public_banks.csv",row.names = FALSE)












