library(XML)
data <- 
  xmlParse("data/troubleshooting/auditory-eeg-352-10-ExperimentAdvisorReport.xml")

xml_data <- xmlToList(data)
