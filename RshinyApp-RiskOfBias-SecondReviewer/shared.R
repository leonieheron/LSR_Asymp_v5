

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



