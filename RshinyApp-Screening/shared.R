# decisions
decisions<-c("No: No reason needed" = 1,
             "No: Publication not on COVID-19" = 11,
             "No: No original data" = 12,
             "No: Data included in other publication" = 13,
             "No: Duplicate" = 14,
             "No: Insufficient extractable data" = 15,
             "No: Study design inappropriate" = 16,
             "No: Infections not diagnosed with a molecular test" = 17,
             "No: Aim of mathematical model not in review scope"=19,
             "No: Inadequate or undocumented follow-up time"=20,
             "No: Other" = 18)

postRedcap = function(data) {
  cat(data)
  result<-postForm(
    uri='https://redcap.ispm.unibe.ch/api/',
    token=API,
    content='record',
    format='csv',
    type='flat',
    overwriteBehavior='overwrite',
    forceAutoNumber='false',
    data=data,
    returnContent='ids',
    returnFormat='csv'
  )
  return(result) # returns id if succesful
}

highlightWords = function(text)
{
  # highlight words with high relevance, use regex to not completly mess up text
  text = gsub('(?i)[.?: ](review|reviews)[ ,]', '<mark style="background-color:#ffee6f">\\1</mark>',    text, perl = T)
  text = gsub('(?i)[.?: ](here)[ ,]', '<mark style="background-color:#ffee6f">\\1</mark>',              text, perl = T)
  text = gsub('(?i)[.?: ](we)[ ,]', '<mark style="background-color:#ffee6f">\\1</mark>',                text, perl = T)
  text = gsub('(?i)[.?: ](mouse|mice)[ ,]', '<mark style="background-color:#ffee6f">\\1</mark>',        text, perl = T)
  text = gsub('(?i)[.?: ](asymp|pre\\-symp|presymp|preclinical|pre\\-clinical|without symptoms|no symptoms|free of symptoms|non\\-symp|nonsymp|symptom\\-free|symptomfree)', '<mark style="background-color:#90EE90">\\1</mark>',        text, perl = T)
  text
}  

# logging function
logfun = function(a, response, redcapdb, logname, addtext = "" )
{
  tryCatch(
    {
      response_id = as.numeric(gsub("[^\\d]+", "", response, perl=TRUE))
    
      
      if(response_id == redcapdb$id[a]){
        
        write.table(data.frame(redcapdb$id[a], "success", response, addtext) , logname, sep = ",", col.names = F,row.names = F, append = T)
      }
      else        {
        write.table(data.frame(redcapdb$id[a], "possible failure", response, addtext) , logname, sep = ",", col.names = F, row.names = F, append = T)
      }
    }, error=function(cond) {write.table(data.frame(redcapdb$id[a], "possible failure", response) , logname, sep = ",", col.names = F, row.names = F, append = T)}
    , warning = function (cond) {write.table(data.frame(redcapdb$id[a], "possible failure", response) , logname, sep = ",", col.names = F, row.names = F, append = T)})
}



