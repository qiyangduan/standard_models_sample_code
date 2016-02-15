# http://anythingbutrbitrary.blogspot.kr/2012/12/a-simple-web-application-using-rook.html

rm(list=ls())
Sys.setenv(LANG = "en")
# install.packages('Rook', dep = TRUE)
library(Rook)

R.server <- Rhttpd$new()
cat("Type:", typeof(R.server), "Class:", class(R.server)) 

churn.app <- function(env) {
    request <- Request$new(env)
    response <- Response$new()

    write.initial.HTML(response, iter)
    response$finish()
}

write.initial.HTML <- function(response, iter) {

    response$write("<h1>Submit to Predict Churners</h1>")
    response$write("<form action=\"/custom/churn_json\" method=\"POST\">") 
    # response$write("JSON data to predict: <input type=\"text\" name=\"json_df\" style=\"font-size:12pt;height:120px;width:400px;\" ><br>")
    response$write("JSON data to predict: <br><textarea name=\"json_df\" cols=\"55\" rows=\"7\"></textarea> <br> <hr>")
    response$write("<input type=\"submit\" value=\"Get Churn Prob Json\" name=\"submit_button\">")
    response$write("</form>")
    response$write("<br>")
}


R.server$add(app = churn.app, name = "churn_app")
print(R.server)

library(jsonlite)

## load the model
load("c:\\tmp\\my_churn_model_rf.rda")

 


write.json_churn.HTML <- function(request, response, churn_model) {
    if ("json_df" %in% names(request$POST())) {

        dfjson = request$POST()$json_df 
        # print(dfjson)
        churn_df = fromJSON(dfjson)
        churn_df$X_churn_flag='?'
        # print(str(churn_df))
        result_df <- churn_df[c("X_customer_id")]

        # Run exactly same transformation as training data.
        # churn_df[churn_df$X_churn_flag == 1,]$X_churn_flag = "T"
        # churn_df[churn_df$X_churn_flag == 0,]$X_churn_flag = "F"
        drops <- c("X_customer_id")
        a_apply_data=churn_df[,!(names(churn_df) %in% drops)]

        apply.predicted<-predict(churn_model,newdata=a_apply_data)
        # print(str(apply.predicted))
        result_df["predicted_churn_flag"] <- apply.predicted

        #response$write("<h1>The input</h1><br>")
        #response$write( request$POST()$json_df )
        #response$write("<hr><h1>The output</h1><br>")
        response$write(toJSON(result_df))
    } else {
        response$write("<h1>could not find  json_df</h1>") 
    }
}


churn.json.app <- function(env) { 
    request <- Request$new(env)
    response <- Response$new()

    write.json_churn.HTML(request, response, churn_model)
    response$finish()
}


R.server$add(app = churn.json.app, name = "churn_json")
R.server$start()
R.server$browse("churn_app")

#print(R.server)
#R.server$stop()
#R.server$start()

# http://www.inside-r.org/packages/cran/Rook/docs/Rhttpd
# R.server$remove( name = "churn_json")
