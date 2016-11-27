require(lsa)

# Load the subreddit vector representations
load("./data/subredsimdata.rdata")

curallnames = rownames(subsimmat)
curallnameslc = tolower(curallnames)
# Function to calculate distance between subreddits
findrelsubreddit <- function(cursubs,curops,numret=nrow(subsimmat)) {
    curvec = 0
    for(i in 1:length(cursubs)) {
            curvec = ifelse(curops[i]=="+",list(curvec + subsimmat[which(curallnameslc==cursubs[i]),]),list(curvec - subsimmat[which(curallnameslc==cursubs[i]),]))[[1]]
    }
    curclosesubs = cosine(x=curvec,y=t(subsimmat))
    curclosesubso = order(curclosesubs,decreasing=TRUE)
return(curclosesubs[curclosesubso][1:10])
}

# Function to validate subreddit inputs
checksubreddits <- function(sub1,sub2,sub3) {
  checknames = curallnameslc
  checknamesplus = c(checknames,"")
  check1 = sum(sub1%in%checknames)!=1
  check2 = sum(sub2%in%checknamesplus)!=1
  check3 = sum(sub3%in%checknamesplus)!=1
  checkreturn = NULL
  if(check1) {
    checkreturn = "Please enter at least one valid subreddit name in the first field." 
  }
  if(check2) {
    checkreturn = "Invalid subreddit name in the second field."
  }
  if(check3) {
    checkreturn = "Invalid subreddit name in the third field."
  }
  if(check2 & check3) {
    checkreturn = "Invalid subreddit name in the second and third field."
  }
return(checkreturn)
}

shinyServer(
function(input, output) {

  data <- reactive({
    cursub1 = tolower(input$sub1)
    cursub2 = tolower(input$sub2)
    cursub3 = tolower(input$sub3)
    validate(
      checksubreddits(cursub1,cursub2,cursub3)
    )
    cursubsh = c(cursub1,cursub2,cursub3)
    cursubs = cursubsh[cursubsh!=""]
    curops = c("+",input$alg1,input$alg2)
    withProgress(message="Calculating related subreddits...",value=0,{relout = findrelsubreddit(cursubs,curops)})
    relout
  })

  output$table <- DT::renderDataTable(DT::datatable({
    cursub1 = tolower(input$sub1)
    cursub2 = tolower(input$sub2)
    cursub3 = tolower(input$sub3)
    cursubsh = c(cursub1,cursub2,cursub3)
    cursubs = cursubsh[cursubsh!=""]
    dhh = data.frame(data())
    baseurls = paste0("http://www.reddit.com/r/",rownames(dhh))
    dh = data.frame(dhh,subredditlink=paste0("<a href='",baseurls,"' target='_blank'>",baseurls,"</a>"))
    d = dh[-which(rownames(dh)%in%cursubs),]
    d
  },escape=FALSE))

}
)
