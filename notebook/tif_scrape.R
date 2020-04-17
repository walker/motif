#loading the package:
# library(xml2)
# library(rvest)
# library(stringr)
# library(jsonlite)
# library(tibble)

#view data 
#head(eats)  

#Combining all the lists to form a data frame
TIF_data <- tibble::tibble()

# TIF_data <- data.frame(row.names = NULL, check.rows = FALSE,
#                       check.names = TRUE, fix.empty.names = TRUE,
#                       stringsAsFactors = FALSE)
# colnames(TIF_data, c("City","Project","Period","PILOTS","EATS","Principal Interest","Assessed Value"))
tif_ids <- c("1430", "822", "2686", "1963", "1253", "823", "5783", "5324", "4869", "4392", "3905", "3442", "3103", "5874", "5384", "4929", "4437", "3748", "3348", "3104", "4337", "3849", "3433", "2933", "2451", "1580", "1250", "824", "5565", "5167", "4618", "4258", "3978", "3513", "3151", "2595", "1586", "1328", "825", "238", "558", "5521", "1871", "1113", "1065", "68", "474", "5584", "5211", "4645", "4261", "3762", "3767", "3172", "2621", "1655", "1376", "1066", "192", "491", "2489", "1860", "1067", "5603", "5146", "4729", "5557", "5120", "4770", "4118", "3830", "2927", "5556", "5117", "4769", "4166", "3932", "3935", "2921", "1158", "1068", "31", "518", "5524", "5236", "4467", "4468", "3790", "2525", "1846", "1069", "5115", "4768", "4164", "3942", "3943", "2924", "2926", "1157", "1070", "284", "497", "2524", "1830", "1525", "1071", "320", "421", "5811", "5812", "5813", "5809", "5810", "5153", "4609", "4247", "3887", "3890", "3198", "2582", "1906", "1335", "1072", "236", "520", "5763", "5093", "4629", "4119", "3952", "3954", "3143", "2605", "1643", "1362", "1073", "111", "437", "5690", "5259", "5866", "5185", "4835", "4360", "3858", "3859", "3165", "2609", "1647", "1366", "1074", "91", "513", "2913", "2732", "1595", "1138", "1075", "89", "3274", "534", "2901", "2695", "1539", "1120", "1076", "568", "5800", "5801", "5339", "4942", "1077", "3408", "2942", "2768", "1721", "1199", "1078", "5606", "5099", "4697", "4180", "3690", "3311", "3122", "2526", "1819", "1147", "1079", "293", "422", "5184", "4589", "4109", "3653", "3292", "2878", "1574", "1209", "1080", "73", "3457", "2945", "2769", "1719", "1200", "1081", "5710", "5281", "4815", "4338", "3816", "3415", "3105", "2699", "1632", "1168", "86", "5773", "5214", "4811", "4335", "3802", "3808", "3161", "2690", "1659", "1379", "198", "449", "5857", "5334", "4847", "4193", "5884", "5394", "4731", "5797", "5311", "4909", "4272", "3744", "5032", "4580", "4147", "3651", "3302", "3065", "2455", "1784", "1264", "1082", "5937", "5208", "4643", "4260", "3983", "3986", "3149", "2619", "1925", "1374", "1083", "240", "566", "1094", "1953", "1084", "2527", "1751", "1451", "1085", "294", "608", "2528", "1789", "1496", "1086", "321", "2529", "1829", "1524", "1087", "322", "423", "5705", "5159", "4613", "4252", "3919", "3920", "3202", "2588", "1578", "1341", "1088", "206", "523", "2494", "1835", "1226", "5688", "5269", "4807", "4332", "3829", "3420", "2882", "2480", "1665", "1287", "1089", "5623", "5227", "4753", "4228", "3736", "3349", "2883", "2481", "1666", "1288", "1090", "5625", "5229", "4754", "4232", "3737", "3350", "2881", "2482", "1663", "1289", "1091", "1092", "5597", "5084", "4738", "4159", "3696", "3314", "2874", "2433", "1592", "1131", "1093")

scrape_tif <- function(tif_id) {

  #Specifying the url for desired website to be scrapped
  url <- paste0("https://app.auditor.mo.gov/TIF/vTIF.aspx?id=", tif_id)

    #Reading the html content from the State Auditor's website
  webpage <- rvest::html_session(url)
  
  #Name of City/County
  city_html <- rvest::html_nodes(webpage, 'span#ContentPlaceHolder1_lblCityCounty')
  city <- rvest::html_text(city_html)
  
  #Name of Plan or Project
  project_html <- rvest::html_nodes(webpage, 'span#ContentPlaceHolder1_lblProjectName')
  project <- rvest::html_text(project_html)
  
  #Report Period
  period_html <- rvest::html_nodes(webpage, 'span#ContentPlaceHolder1_lblReportPeriod')
  period <- rvest::html_text(period_html)
  
  #scrape PILOTS
  pilots_html <- rvest::html_nodes(webpage, 'span#ContentPlaceHolder1_lblPilotInception')
  pilots <- rvest::html_text(pilots_html)
  
  #scrape EATS
  eats_html <- rvest::html_nodes(webpage, 'span#ContentPlaceHolder1_lblEatsInception')
  eats <- rvest::html_text(eats_html)
  
  #scrape Principal and Interest since inception
  PI_html <- rvest::html_nodes(webpage, 'span#ContentPlaceHolder1_lblPrincIncept')
  PI <- rvest::html_text(PI_html)
  
  #scrape Assessed valuation added to the redevelopment project
  AV_html <- rvest::html_nodes(webpage, 'span#ContentPlaceHolder1_lblAssessValueAdded')
  AV <- rvest::html_text(AV_html)
  
  #Add another row of data
  print(c(city,project,period,pilots,eats,PI,AV))
  # return(c("City"=city,"Project"=project,"Period"=period,"PILOTS"=pilots,"EATS"=eats,"Principal Interest"=PI,"Assessed Value"=AV))
  # return(c(city,project,period,pilots,eats,PI,AV))
  return(tibble::tibble("City"=city,"Project"=project,"Period"=period,"PILOTS"=pilots,"EATS"=eats,"Principal Interest"=PI,"Assessed Value"=AV))
}  

lapply(tif_ids, function(x) {
  y <- scrape_tif(x)
  TIF_data <<- bind_rows(TIF_data, y)
})

write.csv(TIF_data, "~/Desktop/TIF_Data.csv")
