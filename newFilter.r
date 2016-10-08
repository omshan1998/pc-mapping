newFilter <- function(mapFilter,hideCountry,countryFilter,filterType,citation,year,gsRank,authors,university,publisher,keywordList){
  debug = FALSE
  if(debug){
    if(!is.null(authors)){
      authors <- unlist(strsplit(authors,","))
      print(authors)
    }

    #print(c("mapFilterType:",mapFilter))
    return(countries)
  }
  

  #This is a middle file between the source and the data source for the map 
  midparticipatory <- countries@data
  #midparticipatory$WORK <- 0
  #midparticipatory$FIRSTPUB <- 0
  #midparticipatory$ALLPUB <- 0
  #midparticipatory$RESTPUB <- 0
  midparticipatory$Count <- 0
  
  

  filteredArticles <- articles
  #filteredArticles <- subset(articles,!(Country.of.Publication..1st.Author. %in% hideCountry))
  #filteredArticles <- subset(filteredArticles, (Country.of.Publication..1st.Author. %in% countryFilter))
  #filteredArticles <- subset(filteredArticles, (Place.of.Publish..1st.author. %in% crossFilter))
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
  #filteredArticles <- subset(filteredArticles, (Place.of.Publish..1st.author. %in% university))
  #filteredArticles <- subset(filteredArticles, (Publisher %in% publisher))
  #filteredArticles <- subset(filteredArticles, (Second.Keyword %in% keywordList))
  
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
    if (midparticipatory$NAME[i] %in% hideCountry){
      midparticipatory$Count[i] <- 0
    }
    
    else if (!is.null(countryFilter)){
      if (midparticipatory$NAME[i] %in% countryFilter){
        print(c("mid name", midparticipatory$NAME[i], "in filter:", countryFilter))
        midparticipatory$Count[i] <- str_count(crossarray,pattern=countryISO2name)
      }
    }
    else {
      midparticipatory$Count[i] <- str_count(crossarray,pattern=countryISO2name)
    }
  }
  
  write.csv(midparticipatory, file = "test.csv")
  
  countries@data <- midparticipatory

  return(countries)
  
}