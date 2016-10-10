newFilter <- function(mapFilter,hideCountry,countryFilter,filterType,citation,year,gsRank,authors,university,publisher,keywordList){
  debug = FALSE
  if(debug){
    if(!is.null(authors)){
      authors <- unlist(strsplit(authors,","))
      print(authors)
    }
    return(countries)
  }
  
  hideCountryList <- isoList[hideCountry] %>% as.character()
  countryList <- isoList[countryFilter] %>% as.character()

  

  #This is a middle file between the source and the data source for the map 
  midparticipatory <- countries@data
  midparticipatory$Count <- 0
  
  

  filteredArticles <- articles
  filteredArticles <- subset(filteredArticles, (Cites %in% c(citation[1]:citation[2])))
  filteredArticles <- subset(filteredArticles, (Year %in% c(year[1]:year[2])))
  filteredArticles <- subset(filteredArticles, (GSRank %in% c(gsRank[1]:gsRank[2])))
  if(!is.null(authors))  {
    pattern = paste(authors,collapse="|")
    filteredArticles <- subset(filteredArticles, (grepl(pattern, Authors)))
  }
  if(!is.null(university)) {
    pattern = paste(university,collapse = "|")
    filteredArticles <- subset(filteredArticles, (grepl(pattern, Place.of.Publish..1st.author.)))
  }
  if(!is.null(keywordList)){
    pattern = paste(keywordList,collapse = "|")
    filteredArticles <- subset(filteredArticles, (grepl(pattern, keywords,ignore.case = TRUE)))
  }
  
  #print(c("before Type filter, there are", dim(filteredArticles)))
  
  # Type filter
  if (!is.null(hideCountry) || !is.null(countryFilter)){
    hideCountryPattern = paste(hideCountryList,collapse = "|")
    countryPattern = paste(countryList,collapse = "|")
    if (filterType == "WORK"){
      if (!is.null(hideCountry)) filteredArticles <- subset(filteredArticles,!grepl(hideCountryPattern, Place.of.Work))
      filteredArticles <- subset(filteredArticles,grepl(countryPattern, Place.of.Work))
    }
    else if (filterType == "FIRSTPUB"){
      if (!is.null(hideCountry)) filteredArticles <- subset(filteredArticles,!grepl(hideCountryPattern, Country.of.Publication..1st.Author.))
      filteredArticles <- subset(filteredArticles,grepl(countryPattern, Country.of.Publication..1st.Author.))
    }
    else if (filterType == "RESTPUB"){
      if (!is.null(hideCountry)) filteredArticles <- subset(filteredArticles,!grepl(hideCountryPattern, Country.of.Publication..Rest.of.authors.))
      filteredArticles <- subset(filteredArticles,grepl(countryPattern, Country.of.Publication..Rest.of.authors.))
    }
    else if (filterType == "ALLPUB"){
      if (!is.null(hideCountry)){
        filteredArticles <- subset(filteredArticles,!grepl(hideCountryPattern, Country.of.Publication..1st.Author.))
        filteredArticles <- subset(filteredArticles,!grepl(hideCountryPattern, Country.of.Publication..Rest.of.authors.))
      }
      filteredArticles <- subset(filteredArticles,(grepl(countryPattern, paste(Country.of.Publication..1st.Author.,Country.of.Publication..Rest.of.authors.))))
    }
  }
  print(c("Filtered article number", dim(filteredArticles)[1]))
  
  
  #filteredArticles <- subset(filteredArticles, (Place.of.Publish..1st.author. %in% university))
  #filteredArticles <- subset(filteredArticles, (Publisher %in% publisher))
  #filteredArticles <- subset(filteredArticles, (Second.Keyword %in% keywordList))
  
  
  #if (filterType == "WORK") filteredArticles <- subset(filteredArticles, (grepl()))
  
  #print(filteredArticles$Authors)

  #mapFilter determines what data to show? "WORK", "FIRSTPUB", "RESTPUB", "ALLPUB"
  if (mapFilter == "WORK") crossarray<-sapply(filteredArticles$Place.of.Work, as.character)
  else if(mapFilter == "FIRSTPUB") crossarray<-sapply(filteredArticles$Country.of.Publication..1st.Author., as.character)
  else if(mapFilter == "RESTPUB") crossarray<-sapply(filteredArticles$Country.of.Publication..Rest.of.authors., as.character)
  else if(mapFilter == "ALLPUB"){
    s<-sapply(filteredArticles$Country.of.Publication..1st.Author., as.character)
    t<-sapply(filteredArticles$Country.of.Publication..Rest.of.authors., as.character)
    crossarray<-paste(s, t)
  }
  crossarray <- paste(crossarray,collapse=",")

  # traverse the middle participatory to count number of articles filtered
  for (i in 1:nrow(midparticipatory)){
    countryISO2name <- midparticipatory[i,"ISO2"] %>% as.character()
    # if (midparticipatory$NAME[i] %in% hideCountry){
    #   midparticipatory$Count[i] <- 0
    # }
    # 
    # else if (!is.null(countryFilter)){
    #   if (midparticipatory$NAME[i] %in% countryFilter){
    #     print(c("mid name", midparticipatory$NAME[i], "in filter:", countryFilter))
    #     midparticipatory$Count[i] <- str_count(crossarray,pattern=countryISO2name)
    #   }
    # }
    # else {
    #   midparticipatory$Count[i] <- str_count(crossarray,pattern=countryISO2name)
    # }
    midparticipatory$Count[i] <- str_count(crossarray,pattern=countryISO2name)
  }
  
  write.csv(midparticipatory, file = "test.csv")
  
  countries@data <- midparticipatory

  return(countries)
}