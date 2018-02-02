# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  stp
# Purpose:      sales training
# programmer:   Anqi Chen
# Date:         20-11-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##-- handle the exception thrown out

  library(DT)
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(digest)
  library(openxlsx)
  library(mongolite)
  library(jsonlite)
  library(utf8)
  
  options(scipen=200,
          mongodb = list(
            "host" = "59.110.31.50:2017"
            # "username" = "root",
            # "password" = "root"
          ))
  
  ## receive signal
  # argss[1] :  R_File_Path
  # argss[2] :  filekey of json
  # argss[3] :  reports save path
  argss <- commandArgs(TRUE)
  R_Json_Path <- argss[1]
  #file_path <- argss[3]
  R_File_Path <- "resource/pre_data_linux.RData"
  load(R_File_Path)

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                              write function
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  

  # writeDown <- function(report){
  #   
  #   wb <- createWorkbook()
  #   
  #   ## 1
  #   addWorksheet(wb, rsd_sheet_names[1])
  #   report7_1 <- cbind("name"="",report$report1_mod1)
  #   colnames(report7_1)[1] <- report_sep_names[1]
  #   writeDataTable(wb, sheet = rsd_sheet_names[1],withFilter = F, report7_1,
  #                  startCol = 1,rowNames = F,colNames = T)
  #   report7_2 <- cbind("name"="",report$report1_mod2)
  #   colnames(report7_2)[1] <- report_sep_names[2]
  #   writeDataTable(wb, sheet = rsd_sheet_names[1],withFilter = F, report7_2,
  #                  startCol = 1,startRow = 8,rowNames = F,colNames = T)
  #   
  #   ## 2
  #   addWorksheet(wb, rsd_sheet_names[2])
  #   report1_1 <- cbind("name"="",report$report2_mod1)
  #   colnames(report1_1)[1] <- report_sep_names[3]
  #   writeDataTable(wb, sheet = rsd_sheet_names[2],withFilter = F, report1_1,
  #                  startCol = 1,rowNames = F,colNames = T)
  #   report1_2 <- bind_rows(report$report2_mod2,
  #                      report$report2_mod3,
  #                      report$report2_mod4,
  #                      report$report2_mod5)
  #   report1_2 <- cbind("name"="",report1_2)
  #   colnames(report1_2)[1] <- report_sep_names[4]
  #   writeDataTable(wb, sheet = rsd_sheet_names[2],withFilter = F, report1_2,
  #                  startCol = 1,startRow = 8,rowNames = F,colNames = T)
  #   
  #   ## 3
  #   addWorksheet(wb, rsd_sheet_names[3])
  #   report2_1 <- cbind("name"="",report$report3_mod1)
  #   colnames(report2_1)[1] <- report_sep_names[5]
  #   writeDataTable(wb, sheet = rsd_sheet_names[3],withFilter = F, report2_1,
  #                  startCol = 1,rowNames = F,colNames = T)
  #   report2_2 <- cbind("name"="",report$report3_mod2)
  #   colnames(report2_2)[1] <- report_sep_names[6]
  #   writeDataTable(wb, sheet = rsd_sheet_names[3],withFilter = F, report2_2,
  #                  startCol = 1,startRow = 9,rowNames = F,colNames = T)
  #   
  #   ## 4
  #   addWorksheet(wb, rsd_sheet_names[4])
  #   report3_1 <- cbind("name"="",report$report4_mod1)
  #   colnames(report3_1)[1] <- report_sep_names[7]
  #   writeDataTable(wb, sheet = rsd_sheet_names[4],withFilter = F, report3_1,
  #                  startCol = 1,rowNames = F,colNames = T)
  #   
  #   
  #   
  #   ## 7
  #   addWorksheet(wb, rsd_sheet_names[5])
  #   report6_1 <- cbind("name"=rep("",50),report$report5_mod1)
  #   colnames(report6_1)[1] <- report_sep_names[8]
  #   writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, report6_1,
  #                  startCol = 1,rowNames = F,colNames = T)
  #   report6_2 <-cbind("name"=rep("",nrow(report$report5_mod2)),report$report5_mod2)
  #   colnames(report6_2)[1] <- report_sep_names[9]
  #   writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, report6_2,
  #                  startCol = 1,startRow = 53,rowNames = F,colNames = T)
  #   report6_3 <- cbind("name"="",report$report5_mod3)
  #   colnames(report6_3)[1] <- report_sep_names[10]
  #   writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, report6_3,
  #                  startCol = 1,startRow = sum(53,3,nrow(report6_2)),rowNames = F,colNames = T)
  #   return(wb)}
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                              curve function
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  curve <- function(curve.no,input_x){
    data <- curve.no
    if (input_x<=min(data$x)) {
      y <- data$y[which.min(data$x)]
    } else if (input_x>=max(data$x)) {
      y <- data$y[which.max(data$x)]
    } else {
      left <- data[which.min(abs(input_x-data$x)),]
      tmp <- data[-which.min(abs(input_x-data$x)),]
      right <- tmp[which.min(abs(input_x-tmp$x)),]
      y <- ifelse(left$x <= right$x,
                  (1-(input_x-left$x)/(right$x-left$x))*left$y + 
                    (1-(right$x-input_x)/(right$x-left$x))*right$y,
                  (1-(input_x-right$x)/(left$x-right$x))*right$y + 
                    (1-(left$x-input_x)/(left$x-right$x))*left$y)}
    
    y
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                      data cleaning part
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## -- hospital view
  # get.data1 <- function(input,phase){
  #   data_decision <- data.frame(
  #     phase = NULL, 
  #     hospital = NULL,
  #     sales_rep = NULL,
  #     product = NULL,
  #     sales_target = NULL,
  #     potential_revenue = NULL,
  #     promotional_budget = NULL,
  #     sr_time_proportion = NULL,
  #     stringsAsFactors = F)
  #   
  #   for (j in 1:10) {
  #     for (q in 1:4){
  #       name.phase = as.character(paste("phase",phase,sep=""))
  #       name.hospital = as.character(unique(hospital_info$name)[j])
  #       name.product = as.character(product_info$product[q])
  #       name.sales_rep <- as.character(input[[paste("p",phase,"_sr_hosp",j,sep="")]])
  #       value.sales_target <- as.numeric(input[[paste("p",phase,"_hosp",j,"_sales_target_",q,sep="")]])
  #       value.promotional_budget <- as.numeric(input[[paste("p",phase,"_promotional_budget_hosp",j,sep="")]])/100*
  #         total_promotional_budget[which(total_promotional_budget$phase==paste(phase_ch,phase,sep="")),]$budget
  #       value.sr_time_proportion <- as.numeric(input[[paste("p",phase,"_hosp",j,"_worktime_",q,sep="")]])/100
  #       
  #       
  #       data_decision <- plyr::rbind.fill(data_decision,data.frame(
  #         phase = name.phase,
  #         hospital = name.hospital,
  #         sales_rep = ifelse(name.sales_rep=="",0,name.sales_rep), 
  #         product = name.product,
  #         sales_target = ifelse(is.na(value.sales_target),0,value.sales_target),
  #         potential_revenue = as.numeric(hospital_info[which(hospital_info$phase==paste(phase_ch,phase,sep="")&
  #                                                              hospital_info$name==name.hospital&
  #                                                              hospital_info$product==name.product),]$potential),
  #         promotional_budget = ifelse(is.na(value.promotional_budget),0,value.promotional_budget),
  #         sr_time_proportion = ifelse(is.null(name.sales_rep)|is.na(value.sr_time_proportion),
  #                                     0,value.sr_time_proportion)
  #       ))
  #     }}
  #   data_decision
  # }
  
  
  get.data1 <- function(input) {
    tmp1 <- input %>%
      left_join(hospital_info, by= c("phase",
                                     "hosp_code",
                                     "prod_name")) %>%   ## need adjust to pro_code in 2.0 version
      left_join(total_promotional_budget, by="phase") %>%
      dplyr::mutate(budget = budget/100*total_budget,
                    prod_hours = prod_hours/100)
    return(tmp1)
      
  }
  
  
  # get.data2 <- function(input,phase){
  #   data_decision2 <- data.frame(
  #     phase = NULL,
  #     sales_rep = NULL,
  #     sales_training = NULL,
  #     product_training = NULL,
  #     field_work = NULL,
  #     meetings_with_team = NULL,
  #     kpi_analysis = NULL,
  #     admin_work = NULL,
  #     work_time = NULL,
  #     stringsAsFactors = F
  #   )
  #   
  #   for (j in 1:5) {
  #     name.sales_rep <- as.character(sr_info_list$sales_rep[j])
  #     value.sales_training <- as.numeric(
  #       input[[paste("p",phase,"_sr",j,"_sales_training",sep="")]])
  #     value.product_training <- as.numeric(
  #       input[[paste("p",phase,"_sr",j,"_product_training",sep="")]])
  #     value.field_work <- as.numeric(
  #       input[[paste("p",phase,"_sr",j,"_field_work",sep="")]])
  #     value.meetings_with_team <- as.numeric(
  #       input[[paste("p",phase,"_flm_team_meeting",sep="")]])
  #     value.kpi_analysis <- as.numeric(
  #       input[[paste("p",phase,"_flm_kpi_analysis",sep="")]])
  #     value.admin_work <- as.numeric(
  #       input[[paste("p",phase,"_flm_admin_work",sep="")]])
  #     
  #     if (name.sales_rep==""){
  #       data_decision2 <- plyr::rbind.fill(data_decision2,data.frame(
  #         phase = as.character(paste(phase_ch,phase,sep="")),
  #         sales_rep = 0,
  #         sales_training = 0,
  #         product_training = 0,
  #         field_work = 0,
  #         meetings_with_team = 0,
  #         kpi_analysis = 0,
  #         admin_work = 0,
  #         work_time = 0))
  #     } else{
  #       
  #       data_decision2 <- plyr::rbind.fill(data_decision2,data.frame(
  #         phase = as.character(paste("phase",phase,sep="")),
  #         sales_rep = name.sales_rep,
  #         sales_training = ifelse(is.na(value.sales_training),0,value.sales_training),
  #         product_training = ifelse(is.na(value.product_training),0,value.product_training),
  #         field_work = ifelse(is.na(value.field_work),0,value.field_work),
  #         meetings_with_team = ifelse(is.na(value.meetings_with_team),0,value.meetings_with_team),
  #         kpi_analysis = ifelse(is.na(value.kpi_analysis),0,value.kpi_analysis),
  #         admin_work = ifelse(is.na(value.admin_work),0,value.admin_work),
  #         work_time = worktime
  #       ))}
  #   }
  #   data_decision2
  # }
  
  get.data2 <- function(input) {
    tmp2 <- data.frame(input,
                       work_time = worktime)
    tmp2 <- rbind(tmp2,
                  rep(0,ncol(tmp2)))
    tmp2$phase <- input$phase[1]
    return(tmp2)
  }
  
  # sales_training <- function(input,phase){sum(c(
  #   as.numeric(input[[paste("p",phase,"_sr1_sales_training",sep = "")]]),
  #   as.numeric(input[[paste("p",phase,"_sr2_sales_training",sep = "")]]),
  #   as.numeric(input[[paste("p",phase,"_sr3_sales_training",sep = "")]]),
  #   as.numeric(input[[paste("p",phase,"_sr4_sales_training",sep = "")]]),
  #   as.numeric(input[[paste("p",phase,"_sr5_sales_training",sep = "")]])),
  #   na.rm = T)}
  # 
  # field_work <- function(input,phase){sum(c(
  #   as.numeric(input[[paste("p",phase,"_sr1_field_work",sep = "")]]),
  #   as.numeric(input[[paste("p",phase,"_sr2_field_work",sep = "")]]),
  #   as.numeric(input[[paste("p",phase,"_sr3_field_work",sep = "")]]),
  #   as.numeric(input[[paste("p",phase,"_sr4_field_work",sep = "")]]),
  #   as.numeric(input[[paste("p",phase,"_sr5_field_work",sep = "")]])),
  #   na.rm = T)}
  

  
  
  
  # get.data3 <- function(input,phase){
  #   flm_decision <- data.frame(
  #     flm_sales_training = sales_training(input,phase),
  #     flm_field_work = field_work(input,phase),
  #     flm_meetings_with_team = ifelse(is.na(as.numeric(input[[paste("p",phase,"_flm_team_meeting",sep = "")]])),
  #                                     0,
  #                                     as.numeric(input[[paste("p",phase,"_flm_team_meeting",sep = "")]])),
  #     flm_kpi_analysis = ifelse(is.na(as.numeric(input[[paste("p",phase,"_flm_kpi_analysis",sep = "")]])),
  #                               0,
  #                               as.numeric(input[[paste("p",phase,"_flm_kpi_analysis",sep = "")]])),
  #     flm_admin_work = ifelse(is.na(as.numeric(input[[paste("p",phase,"_flm_admin_work",sep = "")]])),
  #                             0,
  #                             as.numeric(input[[paste("p",phase,"_flm_admin_work",sep = "")]])),
  #     stringsAsFactors = F)
  #   flm_decision
  #   
  # }
  
  get.data3 <- function(data) {
    flm_decision <- data %>%
      dplyr::select(-product_training, - work_time) %>%
      filter(salesmen!="0") %>%
      gather(project_name,
             days,
             -salesmen,
             -phase) %>%
      group_by(project_name) %>%
      dplyr::summarise(days = sum(days, na.rm = T)) %>%
      spread(project_name,days)
    
    return(flm_decision)
    }
  
  # test <- function(phase,hosp,input) {
  #   get_name <- c(paste("p",phase,"_promotional_budget_hosp",hosp,sep=""),
  #                 paste("p",phase,"_hosp",hosp,"_sales_target_",1:4,sep=""),
  #                 paste("p",phase,"_hosp",hosp,"_worktime_",1:4,sep=""))
  #   chk1 <- (is.null(input[[paste("p",phase,"_sr_hosp",hosp,sep="")]])|   #  check whether sr is null or ""
  #              input[[paste("p",phase,"_sr_hosp",hosp,sep="")]]=="")
  #   chk2 <- any(vapply(get_name,
  #                      function(x) {!is.null(input[[x]])&
  #                          ifelse(is.null(input[[x]]),TRUE,input[[x]]!="")&
  #                          ifelse(is.null(input[[x]]),TRUE,input[[x]]!="0")},logical(1)))
  #   if ( !chk1) {
  #     return(NA)
  #   } else if (chk1&chk2) { return(hosp)
  #   } else {return(NA)}
  # }
  # 
  # calculator <- function(input,phase){
  #   phase1_promotional_budget=0
  #   phase1_total_time_arrangement1 <- 0 
  #   phase1_total_time_arrangement2 <- 0 
  #   phase1_total_time_arrangement3 <- 0 
  #   phase1_total_time_arrangement4 <- 0
  #   phase1_total_time_arrangement5 <- 0
  #   
  #   for(i in 1:10){
  #     
  #     phase1_promotional_budget <-
  #       sum(c(phase1_promotional_budget, 
  #             as.numeric(input[[paste("p",phase,"_promotional_budget_hosp",i,sep="")]])),
  #           na.rm = TRUE)
  #     
  #     tmp <- sum(c(as.numeric(input[[paste("p",phase,"_hosp",i,"_worktime_1",sep="")]]),
  #                  as.numeric(input[[paste("p",phase,"_hosp",i,"_worktime_2",sep="")]]),
  #                  as.numeric(input[[paste("p",phase,"_hosp",i,"_worktime_3",sep="")]]),
  #                  as.numeric(input[[paste("p",phase,"_hosp",i,"_worktime_4",sep="")]])),
  #                na.rm = TRUE)
  #     if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
  #         sr_info_list$sales_rep[1]){
  #       phase1_total_time_arrangement1 <- 
  #         phase1_total_time_arrangement1 +tmp
  #     } else if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
  #                sr_info_list$sales_rep[2]) {
  #       phase1_total_time_arrangement2 <- 
  #         phase1_total_time_arrangement2 +tmp
  #     } else if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
  #                sr_info_list$sales_rep[3]) {
  #       phase1_total_time_arrangement3 <- 
  #         phase1_total_time_arrangement3 +tmp
  #     } else if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
  #                sr_info_list$sales_rep[4]) {
  #       phase1_total_time_arrangement4 <- 
  #         phase1_total_time_arrangement4 +tmp
  #     } else if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
  #                sr_info_list$sales_rep[5]) {
  #       phase1_total_time_arrangement5 <- 
  #         phase1_total_time_arrangement5 +tmp
  #     }
  #   }
  #   
  #   data <- c(phase1_promotional_budget,
  #             phase1_total_time_arrangement1,
  #             phase1_total_time_arrangement2,
  #             phase1_total_time_arrangement3,
  #             phase1_total_time_arrangement4,
  #             phase1_total_time_arrangement5)
  #   data
  #   
  # }
  # 
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                              read data
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  json_mongo <- mongo(collection = "inputs",
              url = sprintf(
                "mongodb://%s/%s",
                # options()$mongodb$username,
                # options()$mongodb$password,
                options()$mongodb$host,
                "TMIST"))
  
  # Read all the entries
  transfer <- json_mongo$find()
  rownames <- which(transfer$uuid==R_Json_Path)
  decision <- transfer[rownames,]$decision[[1]]
  management <- transfer[rownames,]$management[[1]]

  ## arg in need
  phase <- decision$phase[1]
  user_name <- transfer$user_id
  
  decision_input <- apply(decision, 1, function(x) {
  
    part1 <- x$sales
    part2 <- x$visit_hours
    part <- part1 %>%
      left_join(part2, by="prod_name")
    out<-data.frame("hosp_code"=x$hosp_code,
               "hosp_name"=x$hosp_name,
               "phase"=x$phase,
               "budget"=x$budget,
               "salesmen"=x$salesmen,
               part,
               stringsAsFactors = F)
    out$salesmen[which(is.na(out$salesmen))] <- "0"
    return(out)
  })
  
  decision_input <- bind_rows(decision_input)
  
  management_input <- apply(management, 1, function(x) {

    if (x$project_code %in% c(0,1,5)) {
      part <- x$apply
      colnames(part)[which(colnames(part)=="personal")] <- "salesmen"
      part$project_code <-  x$project_code
      part$phase <- x$phase
      
    } else{
      part_m <- x$apply
      part <- data.frame(phase = x$phase,
                         days = part_m$days,
                         salesmen = salesmen_list$salesmen,
                         project_code = x$project_code,
                         stringsAsFactors = F)
    }

    return(part)
  })
  
  management_input <- bind_rows(management_input) %>%
    left_join(project_list, by = "project_code") %>%
    dplyr::select(-project_code) %>%
    spread(project_name,days)
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                              Validation
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  
  # con <- socketConnection(host="localhost", port = 6011, blocking=TRUE,
  #                         server=FALSE, open="r+")
  
  # chk_data <- function(){
  #   calculator_result <- calculator(input,phase)
  #   numberOfhosp <- vapply(1:10,function(x) test(phase,hosp=x,input),c(c=0))
  #   flm_data <- get.data3(input,phase)
  #   
  #   if (sum(numberOfhosp,na.rm=T)>0) {
  #     
  #     warning(paste("error1:",warning_ch[1],paste(numberOfhosp[which(!is.na(numberOfhosp))],collapse=","),warning_ch[2]))
  #     return_value <- "error1"
  #   } else if (calculator_result[1]==0|
  #              calculator_result[2]==0|
  #              calculator_result[3]==0|
  #              calculator_result[4]==0|
  #              calculator_result[5]==0|
  #              calculator_result[6]==0) {
  #     warning("error2")
  #     return_value <- "error2"
  #     
  #   } else if (
  #     calculator_result[1] >100 | 
  #     calculator_result[2] >100 | 
  #     calculator_result[3] >100 | 
  #     calculator_result[4] >100 | 
  #     calculator_result[5] >100 | 
  #     calculator_result[6] >100 | 
  #     sum(flm_data) >worktime
  #   ) {
  #     warning("error3")
  #     return_value <- "error3"
  #     
  #   } else { 
  #     return_value <- NULL}
  #   return(return_value)
  # }
  # 
  # result <- tryCatch({
  #   e <- chk_data()
  # }, warning = function(war) {
  #   
  #   # warning handler picks up where error was generated
  #   print( war)
  #   
  # })
  # 
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                      begin computation
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  db1 <- mongo(collection = "intermedia",
               url = sprintf(
                 "mongodb://%s/%s",
                 # options()$mongodb$username,
                 # options()$mongodb$password,
                 options()$mongodb$host,
                 "TMIST"))
  
  transfer1 <- db1$find()
  
  if (phase ==1) {
  
    # last_report1_1 <- p0_report
    # last_acc_success_value <- 0
    
    rownum <- which(transfer1$uuid=="all"&
                      transfer1$userid=="all" &
                      transfer1$inter$phase==0)
    inter_data <- transfer1[rownum,]$inter$data[[1]]
    last_report1_1 <- transfer1[rownum,]$inter$report[[1]]
    last_acc_success_value <- transfer1[rownum,]$inter$acc_success_value[[1]]
    
  } else {
    
    rownum <- which(transfer1$uuid==R_Json_Path &
                      transfer1$userid==user_name &
                      transfer1$inter$phase==1)
    
    inter_data <- transfer1[rownum,]$inter$data[[1]]
    last_report1_1 <- transfer1[rownum,]$inter$report[[1]]
    last_acc_success_value <- transfer1[rownum,]$inter$acc_success_value[[1]]
   # colnames(last_report1_1) <- as.vector(sapply(colnames(last_report1_1),function(x) as_utf8(x)))
  }
  
  cp_data1 <- get.data1(decision_input)
  cp_data2 <- get.data2(management_input)
  flm_data <- get.data3(cp_data2)
  
  pp_data1 <- inter_data %>% select(hosp_name,
                                hosp_code,
                                prod_name,
                                prod_code,
                                real_revenue,
                                real_volume,
                                sr_sales_performance,
                                deployment_quality_index,
                                customer_relationship_index,
                                promotional_support_index,
                                sales_performance,
                                offer_attractiveness,
                                acc_offer_attractiveness) %>%
    mutate(acc_success_value = last_acc_success_value)%>%
    distinct()
  
  colnames(pp_data1)[5:14] <- paste("pp_",colnames(pp_data1)[5:14],sep="")
  
  pp_data2 <- inter_data %>% select(salesmen,
                                sales_level,
                                real_revenue_by_sr,
                                real_volume_by_sr,
                                sr_acc_revenue,
                                sales_skills_index,
                                product_knowledge_index,
                                motivation_index,
                                sr_acc_field_work,
                                target_revenue_realization_by_sr) %>%
    distinct()
  
  colnames(pp_data2)[3:10] <- paste("pp_",colnames(pp_data2)[3:10],sep="")
  
  
  
  ##----------------------------------------------------------------------------
  ##--                 data processing
  ##----------------------------------------------------------------------------
  calculation <- function(pp_data1,
                          pp_data2,
                          cp_data1,
                          cp_data2){
    #
    #
    tmp1 <- left_join(cp_data1,dplyr::select(pp_data1,-prod_code,-hosp_code),by=c("hosp_name","prod_name"))
    tmp2 <- left_join(cp_data2,pp_data2,by=c("salesmen"))
    
    tmp <- left_join(tmp1,tmp2,by=c("phase","salesmen")) %>%
      dplyr::mutate(salesmen = ifelse(prod_hours==0,"0",salesmen),
                    sales_level = ifelse(prod_hours==0,"0",sales_level),
                    pp_real_volume_by_sr = ifelse(prod_hours==0,0,pp_real_volume_by_sr),
                    pp_real_revenue_by_sr = ifelse(prod_hours==0,0,pp_real_revenue_by_sr),
                    pp_sr_acc_revenue = ifelse(prod_hours==0,0,pp_sr_acc_revenue),
                    pp_sales_skills_index = ifelse(prod_hours==0,0,pp_sales_skills_index),
                    pp_product_knowledge_index = ifelse(prod_hours==0,0,pp_product_knowledge_index),
                    pp_motivation_index = ifelse(prod_hours==0,0,pp_motivation_index),
                    pp_sr_acc_field_work = ifelse(prod_hours==0,0,pp_sr_acc_field_work),
                    pp_target_revenue_realization_by_sr = ifelse(prod_hours==0,0,pp_target_revenue_realization_by_sr),
                    sales_training = ifelse(prod_hours==0,0,sales_training),
                    product_training = ifelse(prod_hours==0,0,product_training),
                    field_work = ifelse(prod_hours==0,0,field_work),
                    meetings_with_team = ifelse(prod_hours==0,0,meetings_with_team),
                    kpi_analysis = ifelse(prod_hours==0,0,kpi_analysis),
                    admin_work = ifelse(prod_hours==0,0,admin_work),
                    work_time = ifelse(prod_hours==0,0,work_time)) %>%
      dplyr::mutate(product_price = sapply(prod_name,function(x) product_info[which(product_info$prod_name==x),]$price),
                    target_revenue= prod_hours,
                    target_volume = round(target_revenue/product_price)) %>%
      group_by(phase,salesmen) %>%
      dplyr::mutate(other_time=work_time-(
        product_training+
          meetings_with_team),
        sr_time=prod_hours*other_time,
        no_hospitals = n_distinct(hosp_name),
        sr_time_total=sum(sr_time,na.rm=T),
        last_revenue_by_sr = sum(pp_real_revenue,na.rm=T)) %>%
      ungroup %>%
      group_by(phase,hosp_name) %>%
      dplyr::mutate(sr_time_by_hosp=sum(sr_time,na.rm=T)) %>%
      ungroup() %>%
      dplyr::mutate(product_time_proportion=round(sr_time/ifelse(sr_time_by_hosp==0,0.0001,sr_time_by_hosp),2),
                    budget = round(budget*product_time_proportion),
                    promotional_factor = ifelse(target_revenue==0,0,round(budget/target_revenue*100,2)),
                    sr_acc_field_work = pp_sr_acc_field_work+field_work,
                    overhead_factor = sapply(pp_motivation_index,function(x) curve(curve12,x)),
                    overhead_time = round(overhead_factor*overhead,0),
                    real_sr_time = round(sr_time-overhead_time*prod_hours,2),
                    pp_experience_index = round(sapply(pp_sr_acc_revenue,function(x) round(curve(curve11,x),2))),
                    field_work_peraccount = field_work/ifelse(no_hospitals==0,0.0001,no_hospitals),
                    product_knowledge_addition_current_period = sapply(product_training,function(x)curve(curve26,x)),
                    product_knowledge_transfer_value = sapply(pp_product_knowledge_index,function(x)curve(curve28,x)),
                    ss_accumulated_field_work_delta = sapply(sr_acc_field_work,function(x)curve(curve42,x)),
                    ss_accumulated_sales_training_delta = sapply(sales_training,function(x)curve(curve43,x)),
                    ss_experience_index_pp = sapply(pp_experience_index,function(x)curve(curve44,x)),
                    m_sales_training_delta = sapply(sales_training,function(x)curve(curve17,x)),
                    m_admin_work_delta = sapply(admin_work,function(x)curve(curve18,x)))%>%
      dplyr::mutate(sales_skills_index = round(
        (ss_accumulated_field_work_delta+pp_sales_skills_index)*((weightage$sales_skills)$field_work)+
          (ss_accumulated_sales_training_delta+pp_sales_skills_index)*((weightage$sales_skills)$sales_training)+
          (ss_experience_index_pp+pp_sales_skills_index)*((weightage$sales_skills)$experience)),
        product_knowledge_index = round(
          product_knowledge_addition_current_period+
            pp_product_knowledge_index)) %>%
      dplyr::mutate(srsp_motivation_delta = sapply(pp_motivation_index,function(x)curve(curve32,x)),
                    srsp_sales_skills_delta = sapply(sales_skills_index,function(x)curve(curve34,x)),
                    srsp_product_knowledge_delta = sapply(product_knowledge_index,
                                                          function(x)curve(curve33,x)),
                    srsp_time_with_account_delta =  mapply(function(x,y,z){ if(
                      x==as.character(product_info$prod_name[2])){
                      curve(curve36,y)} else if (
                        x==as.character(product_info$prod_name[3])) {
                        curve(curve37,y)} else if (x==as.character(product_info$prod_name[4])) {
                          curve(curve38,y)} else if (x==as.character(product_info$prod_name[1])&
                                                     z %in% big_hos_list){
                            curve(curve39,y)} else if (x==as.character(product_info$prod_name[1])&
                                                       z ==big_big_hos_list){
                              curve(curve39_1,y)}else{curve(curve35,y)}},
                      prod_name,real_sr_time,hosp_name)) %>%
      dplyr::mutate(sr_sales_performance =
                      (srsp_motivation_delta+pp_sr_sales_performance)*
                      ((weightage$sr_sales_performance)$motivation)+
                      (srsp_sales_skills_delta+pp_sr_sales_performance)*
                      ((weightage$sr_sales_performance)$sales_skills)+
                      (srsp_product_knowledge_delta+pp_sr_sales_performance)*
                      ((weightage$sr_sales_performance)$product_knowledge)+
                      (srsp_time_with_account_delta+pp_sr_sales_performance)*
                      ((weightage$sr_sales_performance)$time_with_account))%>%
      dplyr::mutate(sr_sales_performance = ifelse(sr_sales_performance<0,0,sr_sales_performance),
                    dq_admin_work_delta = sapply(admin_work,function(x)curve(curve5,x)),
                    dq_meetings_with_team_delta =sapply(meetings_with_team,function(x)curve(curve7,x)),
                    dq_kpi_analysis_factor = sapply(kpi_analysis,function(x)curve(curve8,x)))%>%
      dplyr::mutate(deployment_quality_index = round(
        (pp_deployment_quality_index+dq_admin_work_delta)*
          ((weightage$deployment_quality)$admin_work)+
          (pp_deployment_quality_index+dq_meetings_with_team_delta)*
          ((weightage$deployment_quality)$meetings_with_team)+
          pp_deployment_quality_index*dq_kpi_analysis_factor*
          ((weightage$deployment_quality)$kpi_report_analysis)))%>%
      dplyr::mutate(deployment_quality_index = ifelse(deployment_quality_index<0,0,deployment_quality_index),
                    ps_promotional_budget_factor = sapply(promotional_factor,function(x)curve(curve30,x))) %>%
      dplyr::mutate(promotional_support_index = 
                      pp_promotional_support_index*ps_promotional_budget_factor) %>%
      dplyr::mutate(promotional_support_index = ifelse(promotional_support_index<0,0,promotional_support_index),
                    sp_field_work_delta = sapply(field_work_peraccount,function(x)curve(curve40,x)),
                    sp_deployment_quality_delta = sapply(deployment_quality_index,function(x)curve(curve41,x))) %>%
      dplyr::mutate(sales_performance = 
                      sr_sales_performance*((weightage$sales_performance)$sr_sales_performance)+
                      (pp_sales_performance+sp_field_work_delta)*
                      ((weightage$sales_performance)$field_work)+
                      (pp_sales_performance+sp_deployment_quality_delta)*
                      ((weightage$sales_performance)$deployment_quality))%>%
      dplyr::mutate(sales_performance = ifelse(sales_performance<0,0,sales_performance),
                    cr_product_knowledge_delta = 
                      sapply(product_knowledge_index,function(x)curve(curve2,x)),
                    cr_promotional_support_delta = 
                      sapply(ps_promotional_budget_factor,function(x)curve(curve3,x)),
                    cr_pp_customer_relationship_index = 
                      sapply(pp_customer_relationship_index,function(x)curve(curve4,x)))%>%
      dplyr::mutate(customer_relationship_index = 
                      (cr_pp_customer_relationship_index+cr_product_knowledge_delta)*
                      (weightage$customer_relaitonship)$product_knowledge+
                      (cr_pp_customer_relationship_index+cr_promotional_support_delta)*
                      (weightage$customer_relaitonship)$promotional_support) %>%
      dplyr::mutate(customer_relationship_index=ifelse(customer_relationship_index<0,0,customer_relationship_index))%>%
      dplyr::mutate(oa_customer_relationship_factor = 
                      mapply(function(x,y){if (x==as.character(product_info$prod_name[1])){
                        curve(curve19,y)} else if(
                          x==as.character(product_info$prod_name[2])){
                          curve(curve20,y)} else if (
                            x==as.character(product_info$prod_name[3])) {
                            curve(curve21,y)} else {
                              curve(curve22,y)}},
                        prod_name,customer_relationship_index),
                    oa_sales_performance_factor = sapply(sales_performance,function(x)curve(curve25,x))) %>%
      dplyr::mutate(cp_offer_attractiveness = 
                      oa_customer_relationship_factor*100*
                      (weightage$cp_offer_attractiveness)$customer_relationship+
                      oa_sales_performance_factor*100*
                      (weightage$cp_offer_attractiveness)$sales_performance) %>%
      dplyr::mutate(cp_offer_attractiveness = ifelse(salesmen==0,0,cp_offer_attractiveness),
                    offer_attractiveness = 
                      cp_offer_attractiveness*(weightage$total_attractiveness)$cp_offer_attractiveness+
                      pp_offer_attractiveness*(weightage$total_attractiveness)$pp_offer_attractiveness,
                    acc_offer_attractiveness = round(pp_acc_offer_attractiveness+offer_attractiveness),
                    market_share =  mapply(function(x,y){if (x==as.character(product_info$prod_name[1])){
                      curve(curve51_1,y)} else if(
                        x==as.character(product_info$prod_name[2])){
                        curve(curve51_2,y)} else if (
                          x==as.character(product_info$prod_name[3])) {
                          curve(curve51_3,y)} else {
                            curve(curve51_4,y)}},
                      prod_name,offer_attractiveness),
                    real_revenue = round(market_share/100*potential),
                    real_volume = round(real_revenue/product_price)) %>%
      ungroup() %>%
      dplyr::group_by(phase,salesmen) %>%
      dplyr::mutate(target_revenue_by_sr = sum(target_revenue,na.rm=T),
                    target_revenue_percent = target_revenue_by_sr/last_revenue_by_sr,
                    bonus_factor = sapply(target_revenue_percent,function(x) {if (x==0|is.nan(x)) {
                      0 }  else {1}}),
                    real_revenue_by_sr = sum(real_revenue,na.rm=T),
                    target_revenue_realization_by_sr = round(real_revenue_by_sr/target_revenue_by_sr*100,2),
                    target_volume_by_sr = sum(target_volume,na.rm=T),
                    real_volume_by_sr = sum(real_volume,na.rm=T),
                    target_volume_realization_by_sr = round(real_volume_by_sr/target_volume_by_sr*100,2),
                    bonus_tmp = mapply(function(x,y) {if(is.nan(x)) {
                      0} else if (x >= 90 & x <= 120){
                        round(x/100*y*0.03)} else if(x >120) {
                          round(1.2*y*0.03)} else {0}},
                      target_revenue_realization_by_sr,real_revenue_by_sr),
                    bonus = round(bonus_tmp*bonus_factor),
                    sr_acc_revenue = real_revenue_by_sr+pp_sr_acc_revenue,
                    experience_index = round(sapply(sr_acc_revenue, function(x) round(curve(curve11,x),2))),
                    m_meeting_with_team_delta =  mapply(function(x,y){
                      if (x == "junior") {
                        curve(curve13,y)
                      } else if(x=="middle"){
                        curve(curve14,y)
                      } else if(x=="senior"){
                        curve(curve15,y)
                      } else{0}
                    },sales_level,
                    meetings_with_team,SIMPLIFY=T),
                    m_sales_target_realization_delta = sapply(target_revenue_realization_by_sr,function(x) 
                      if (!is.nan(x)) {curve(curve16,x)} else {0}),
                    motivation_index = round(
                      (pp_motivation_index+m_admin_work_delta)*
                        ((weightage$motivation)$admin_work)+
                        (pp_motivation_index+m_sales_target_realization_delta)*
                        ((weightage$motivation)$sales_target_realization)+
                        (pp_motivation_index+m_meeting_with_team_delta)*
                        ((weightage$motivation)$meetings_with_team)+
                        (pp_motivation_index+m_sales_training_delta)*
                        ((weightage$motivation)$sales_training))) %>%
      dplyr::mutate(motivation_index=ifelse(salesmen==0,0,motivation_index)) %>%
      ungroup()
    
    
    tmp
  }
  
  
  data_to_use <- calculation(pp_data1,
                             pp_data2,
                             cp_data1,
                             cp_data2)
  
  
  ##----------------------------------------------------------------------------
  ##--                 making reports
  ##----------------------------------------------------------------------------
  
  report_data <- function(tmp,flm_data) {
    
    tmp1 <- tmp %>% 
      dplyr::mutate(prod_name = factor(prod_name,levels=product_level)) 
    tmp2 <- tmp %>%
      filter(salesmen!="0") %>%
      dplyr::mutate(salesmen=factor(salesmen,levels = sr_rep_level))
    
    ## report 1
    profit_tmp <- tmp1 %>%
      select(real_revenue,
             real_volume,
             budget,
             prod_name) %>%
      dplyr::mutate(production_cost = sapply(prod_name,function(x)product_info[which(product_info$prod_name==x),]$cost),
                    production_fee = round(production_cost*real_volume),
                    total_revenue =round(sum(real_revenue,na.rm=T)),
                    total_production_fee =round(sum(production_fee,na.rm=T)),
                    total_promotional_budget = round(sum(budget,na.rm=T)),
                    total_profit = total_revenue-total_production_fee-total_promotional_budget)  %>%
      select(total_profit) %>%
      distinct()
    
    
    report1_mod1 <- tmp1 %>%
      select(phase,
             salesmen,
             hosp_name,
             product_knowledge_index,
             sales_skills_index,
             customer_relationship_index,
             motivation_index,
             real_revenue,
             pp_acc_success_value) %>%
      distinct() %>%
      dplyr::mutate(total_revenue = round(sum(real_revenue,na.rm=T),2)) %>%
      filter(salesmen!="0") %>%
      dplyr::mutate(average_customer_relationship_index = round(mean(customer_relationship_index,na.rm=T),2),
                    average_sales_skills_index = round(mean(sales_skills_index,na.rm=T),2),
                    average_product_knowledge_index = round(mean(product_knowledge_index,na.rm=T),2),
                    average_motivation_index = round(mean(motivation_index,na.rm=T),2),
                    team_capability = round((average_motivation_index +
                                               average_product_knowledge_index +
                                               average_sales_skills_index)/3)) %>%
      select(phase,
             total_revenue,
             average_customer_relationship_index,
             team_capability,
             pp_acc_success_value) %>%
      distinct() %>%
      dplyr::mutate(profit=as.numeric(profit_tmp),
                    inter1=(weightage$success_value)$total_sales*curve(curve50,total_revenue),
                    inter2=(weightage$success_value)$team_capability*curve(curve46,team_capability),
                    inter3=(weightage$success_value)$contribution_margin*curve(curve49,profit),
                    success_value = round(inter1+inter2+inter3),
                    acc_success_value = success_value + pp_acc_success_value) %>%
      dplyr::mutate(success_value = ifelse(phase==paste(phase_ch,"0",sep=""),"",success_value),
                    acc_success_value = ifelse(phase==paste(phase_ch,"0",sep=""),"",acc_success_value)) %>%
      select(phase,
             total_revenue,
             profit,
             team_capability,
             success_value,
             acc_success_value) %>%
      distinct() %>%
      dplyr::mutate(phase = paste("phase",phase,sep=""))
    
    acc_success_value <- ifelse(is.na(as.numeric(report1_mod1$acc_success_value)),0,
                                as.numeric(report1_mod1$acc_success_value))
    
    colnames(report1_mod1) <- colname_1_1
    

    
    ## report 1——2
    report1_mod2 <- tmp1 %>%
      select(hosp_name,
             hosp_code,
             prod_name,
             real_revenue,
             pp_real_revenue) %>%
      group_by(hosp_name) %>%
      dplyr::mutate(hospital_revenue = round(sum(real_revenue,na.rm=T)),
                    pp_hospital_revenue = round(sum(pp_real_revenue,na.rm=T))) %>%
      ungroup() %>%
      select(hosp_name,
             hosp_code,
             pp_hospital_revenue,
             hospital_revenue) %>%
      distinct() %>%
      arrange(hosp_code) %>%
      select(-hosp_code)
    
    colnames(report1_mod2) <- colname_1_2

    
    ## report 2——1
    report2_mod1 <- tmp2 %>%
      group_by(salesmen) %>%
      dplyr::mutate(visit_time=round(sum(real_sr_time,na.rm=T)),
                    total_sr_time=round(overhead_time+
                                          product_training+
                                          meetings_with_team+
                                          visit_time)) %>%
      ungroup() %>%
      select(overhead_time,
             product_training,
             meetings_with_team,
             visit_time,
             total_sr_time,
             salesmen) %>%
      distinct()
    
    colnames(report2_mod1) <- colname_2_1
    
    report2_mod1 <- report2_mod1 %>%
      gather(variable,value,-salesmen) %>%
      spread(salesmen,value) 
    
    report2_rank1 <- data.frame(
      variable=colname_2_1[1:5],
      rank=1:5,
      stringsAsFactors = F
    )
    
    report2_mod1 <- report2_mod1 %>%
      left_join(report2_rank1,by="variable") %>%
      arrange(rank) %>%
      dplyr::select(variable,
             one_of(sr_rep_level))
    
    colnames(report2_mod1) <- c(name_ch,sr_rep_level)
      
  
    
    
    
    ## report 2——2
    report2_mod2 <- tmp2 %>%
      select(salesmen,
             pp_product_knowledge_index,
             product_knowledge_index) %>%
      distinct()
    
    colnames(report2_mod2) <- colnames_2_2
    
    report2_mod2 <- report2_mod2 %>%
      gather(variable,value,-sr_rep) %>%
      spread(sr_rep,value) %>%
      select(variable,
            one_of(sr_rep_level))
    
    colnames(report2_mod2) <- c(name_ch,sr_rep_level)
    
    
    ## report 2——3
    report2_mod3 <- tmp2 %>%
      select(pp_experience_index,
             experience_index,
             salesmen) %>%
      distinct()
    
    colnames(report2_mod3) <- colnames_2_3
    
    report2_mod3 <- report2_mod3 %>%
      gather(variable,value,-sr_rep) %>%
      spread(sr_rep,value)  %>%
      select(variable,
             one_of(sr_rep_level))
    
    colnames(report2_mod3) <- c(name_ch,sr_rep_level)
    
   
    
    ## report 2——4
    report2_mod4 <- tmp2 %>%
      select(salesmen,
             pp_sales_skills_index,
             sales_skills_index) %>%
      distinct()
    
    colnames(report2_mod4) <- colnames_2_4
    
    report2_mod4 <- report2_mod4 %>%
      gather(variable,value,-sr_rep) %>%
      spread(sr_rep,value)  %>%
      select(variable,
             one_of(sr_rep_level))
    
    colnames(report2_mod4) <- c(name_ch,sr_rep_level)
    
    
    ## report 2——5
    report2_mod5 <- tmp2 %>%
      select(salesmen,
             pp_motivation_index,
             motivation_index) %>%
      distinct()  
    
    colnames(report2_mod5) <- colnames_2_5
    
    report2_mod5 <- report2_mod5 %>%
      gather(variable,value,-sr_rep) %>%
      spread(sr_rep,value)  %>%
      select(variable,
             one_of(sr_rep_level))
    
    colnames(report2_mod5) <- c(name_ch,sr_rep_level)
    
    
    ## report 3——1
    flm_report <- tmp1 %>%
      select(salesmen,
             bonus) %>%
      distinct() %>%
      dplyr::mutate(all_sr_bonus=sum(bonus,na.rm=T)) %>%
      select(all_sr_bonus) %>%
      distinct()
    
    flm_report <- flm_data %>%
      dplyr::mutate(all_sr_bonus = flm_report$all_sr_bonus,
                    work_time=sales_training+
                      field_work+
                      meetings_with_team+
                      kpi_analysis+
                      admin_work)
    
    report3_mod1 <- tmp1 %>%
      filter(salesmen!=0) %>%
      select(salesmen,bonus) %>%
      distinct() %>%
      dplyr::mutate(salesmen=factor(salesmen,levels = sr_rep_level)) %>%
      do(plyr::rbind.fill(.,data.frame(salesmen=over_ch,
                                       bonus = sum(.$bonus)))) %>%
      arrange(salesmen) 
    
    colnames(report3_mod1) <- colnames_3_1
    
    report3_mod2 <- flm_report %>%
      select(-all_sr_bonus) 
    
    colnames(report3_mod2) <- colnames_3_2
    
    report3_mod2 <- report3_mod2 %>%
      gather(variable,value)  %>%
      select(variable,
             value)
    
    colnames(report3_mod2) <- c(name_ch,value_ch)
    
    
    
    ## report 4——1
    report4_mod1 <- tmp1 %>%
      select(hosp_name,
             prod_name,
             salesmen,
             sr_time,
             real_sr_time,
             budget,
             real_revenue,
             real_volume,
             hosp_code) %>%
      group_by(hosp_name) %>%
      dplyr::mutate(production_cost = sapply(prod_name,function(x)product_info[which(product_info$prod_name==x),]$cost),
                    production_fee = round(production_cost*real_volume),
                    profit = real_revenue - budget - production_fee) %>%
      select(hosp_name,
             prod_name,
             salesmen,
             sr_time,
             real_sr_time,
             real_revenue,
             budget,
             production_fee,
             profit,
             hosp_code) %>%
      do(plyr::rbind.fill(.,data.frame(hosp_name = first(.$hosp_name),
                                       prod_name = over_ch,
                                       salesmen = first(.$salesmen),
                                       sr_time = sum(.$sr_time,na.rm=T),
                                       real_sr_time = sum(.$real_sr_time,na.rm=T),
                                       real_revenue = sum(.$real_revenue,na.rm=T),
                                       budget = sum(.$budget,na.rm=T),
                                       production_fee = sum(.$production_fee,na.rm=T),
                                       profit = sum(.$profit,na.rm=T),
                                       hosp_code = first(.$hosp_code)))) %>%
      ungroup()
    
    
    colnames(report4_mod1) <- colnames_4_1
    
    report4_mod1 <- report4_mod1 %>%
      gather(variable,value,one_of(colnames_4_1[3:9])) %>%
      spread(prod_name,value) %>%
      left_join(report4_rank1,by="variable") %>%
      arrange(hosp_code,rank) %>%
      select(one_of(variable_list_4_1))
    
    colnames(report4_mod1) <- colnames_4_1_m
    
 
    
    
    ## report 5——1
    report5_mod1 <- tmp1 %>%
      select(hosp_name,
             hosp_code,
             prod_name,
             real_revenue,
             pp_real_revenue,
             target_revenue)%>%
      group_by(hosp_name) %>%
      do(plyr::rbind.fill(.,data.frame(hosp_name = first(.$hosp_name),
                                       hosp_code = first(.$hosp_code),
                                       prod_name = over_ch,
                                       real_revenue=sum(.$real_revenue,na.rm=T),
                                       pp_real_revenue=sum(.$pp_real_revenue,na.rm=T),
                                       target_revenue = sum(.$target_revenue,na.rm=T)))) %>%
      dplyr::mutate(real_revenue_increase = real_revenue - pp_real_revenue,
                    real_revenue_increase_ratio = round(real_revenue_increase/pp_real_revenue*100,4),
                    target_revenue_realization = round(real_revenue/target_revenue*100,4)) %>%
      arrange(hosp_code) %>%
      select(hosp_name, 
             prod_name,
             target_revenue,
             pp_real_revenue,
             real_revenue,
             real_revenue_increase,
             real_revenue_increase_ratio,
             target_revenue_realization)
    
    
    
    colnames(report5_mod1) <- colnames_5_1
    
    
 
    report5_mod2 <- tmp1 %>%
      filter(salesmen!=0) %>%
      select(salesmen,
             prod_name,
             real_revenue,
             pp_real_revenue,
             target_revenue) %>%
      group_by(salesmen,prod_name) %>%
      dplyr::summarise(real_revenue_by_sr = sum(real_revenue,na.rm=T),
                       pp_real_revenue_by_sr = sum(pp_real_revenue,na.rm=T),
                       target_revenue_by_sr = sum(target_revenue,na.rm=T)) %>%
      do(plyr::rbind.fill(.,data.frame(salesmen=first(.$salesmen),
                                       prod_name =over_ch,
                                       real_revenue_by_sr=sum(.$real_revenue_by_sr,na.rm=T),
                                       pp_real_revenue_by_sr =sum(.$pp_real_revenue_by_sr,na.rm=T),
                                       target_revenue_by_sr=sum(.$target_revenue_by_sr,na.rm=T)))) %>%
      dplyr::mutate(sr_target_revenue_realization = round(real_revenue_by_sr/target_revenue_by_sr*100,4)) %>%
      select(salesmen,
             prod_name,
             target_revenue_by_sr,
             pp_real_revenue_by_sr,
             real_revenue_by_sr,
             sr_target_revenue_realization) %>%
      arrange(salesmen)
    
    
    colnames(report5_mod2) <- colnames_5_2
    
    report5_mod3 <- tmp1 %>%
      select(prod_name,
             real_revenue,
             pp_real_revenue,
             target_revenue) %>%
      group_by(prod_name) %>%
      dplyr::summarise(real_revenue_by_product = round(sum(real_revenue,na.rm=T)),
                       pp_real_revenue_by_product = round(sum(pp_real_revenue,na.rm=T)),
                       real_revenue_increase = round(real_revenue_by_product - pp_real_revenue_by_product),
                       target_revenue_by_product = round(sum(target_revenue,na.rm=T))) %>%
      do(plyr::rbind.fill(.,data.frame(prod_name=over_ch,
                                       real_revenue_by_product=round(sum(.$real_revenue_by_product,na.rm=T)),
                                       pp_real_revenue_by_product=round(sum(.$pp_real_revenue_by_product,na.rm=T)),
                                       real_revenue_increase=sum(.$real_revenue_increase,na.rm=T),
                                       target_revenue_by_product=round(sum(.$target_revenue_by_product,na.rm=T))))) %>%
      dplyr::mutate(real_revenue_increase_ratio = ifelse(is.nan(round(real_revenue_increase/pp_real_revenue_by_product*100,2)),0,
                                                         round(real_revenue_increase/pp_real_revenue_by_product*100,4)),
                    target_revenue_realization_by_product = ifelse(is.nan(round(real_revenue_by_product/target_revenue_by_product*100,2)),0,
                                                                   round(real_revenue_by_product/target_revenue_by_product*100,4))) %>%
      select(prod_name,
             target_revenue_by_product,
             pp_real_revenue_by_product,
             real_revenue_by_product,
             real_revenue_increase,
             real_revenue_increase_ratio,
             target_revenue_realization_by_product)
    
    
    report5_mod3 <- report5_mod3 %>%
      left_join(dplyr::select(product_info,prod_code,prod_name),by="prod_name") %>%
      arrange(prod_code) %>%
      select(-prod_code)
    
    colnames(report5_mod3) <- colnames_5_3
    
   
    
    
    out<-list("report1_mod1"=report1_mod1,
              "report1_mod2"=report1_mod2,
              "report2_mod1"=report2_mod1,
              "report2_mod2"=report2_mod2,
              "report2_mod3"=report2_mod3,
              "report2_mod4"=report2_mod4,
              "report2_mod5"=report2_mod5,
              "report3_mod1"=report3_mod1,
              "report3_mod2"=report3_mod2,
              "report4_mod1"=report4_mod1,
              "report5_mod1"=report5_mod1,
              "report5_mod2"=report5_mod2,
              "report5_mod3"=report5_mod3,
             "acc_success_value"=acc_success_value
              
    )
    
    out
    
  }
  
  data_to_use2 <- report_data(data_to_use,flm_data)
  
  
  
  if (phase == 1) {
    last_report1_1$phase <- c("phase0","phase1","phase2")
    report1_1_tmp <- rbind(last_report1_1[1,],data_to_use2$report1_mod1)
  } else {
    last_report1_1$phase <- c("phase0","phase1")
    report1_1_tmp <- rbind(last_report1_1[1:2,],data_to_use2$report1_mod1)
  }
  
  
  report1_mod1 <- report1_1_tmp %>%
    #dplyr::mutate(phase = paste("??????",phase,sep="")) %>%
    gather(name,value,-phase) %>%
    spread(phase,value)  %>%
    left_join(report7_mod1_rank,by="name") %>%
    arrange(rank) %>%
    select(-variable,-rank) 
  
  if (phase == 1) {
    report1_mod1 <- data.frame(name = report1_mod1$name,
                               phase0 = report1_mod1$phase0,
                               phase1= report1_mod1$phase1,
                               phase2= rep(NA,5))
    
    
  } else {
    report1_mod1 <- data.frame(name = report1_mod1$name,
                               phase0 = report1_mod1$phase0,
                               phase1= report1_mod1$phase1,
                               phase2= report1_mod1$phase2)
  }
 
  colnames(report1_mod1) <- colnames_6_1
  report1_mod1[is.na(report1_mod1)] <- -1
  
####-- report data  
  tmp_data <- list(
    "report1_finalreport" = report1_mod1,
    "report1_sales_report" = data_to_use2$report1_mod2,
    "report2_staff_timetable" = data_to_use2$report2_mod1,
    "report2_product_knowledge" = data_to_use2$report2_mod2,
    "report2_experience" = data_to_use2$report2_mod3,
    "report2_sales_skills" = data_to_use2$report2_mod4,
    "report2_motivation" = data_to_use2$report2_mod5,
    "report3_staff_cost" = data_to_use2$report3_mod1,
    "report3_flm_timetable" = data_to_use2$report3_mod2,
    "report4_resource" = data_to_use2$report4_mod1,
    "report5_sales_by_hosp" = data_to_use2$report5_mod1,
    "report5_sales_by_salesmen" = data_to_use2$report5_mod2,
    "report5_sales_by_prod" = data_to_use2$report5_mod3)
  
  names(tmp_data) <- report_name
  
  to_mongo_tmp <- lapply(1:length(tmp_data), function(x) {
    report_name <- names(tmp_data)[x]
    tmp <- tmp_data[[x]]
    colname <- colnames(tmp)
    colnames(tmp) <- 
      sapply(colname,function(x) names(names_box[which(x==names_box)]))
    out_new <- list("phase"=phase,
                    report_name,
                    "result"=tmp)
    names(out_new)[2] <- "report_name"
    return(out_new)
  })
  
  to_mongo <- list(uuid=R_Json_Path,
                   user_id=user_name,
                   "report"=to_mongo_tmp)
  
#####-- intermedia data
  if (R_Json_Path %in% transfer1$uuid) {
    
    mongo_tmp <- paste('{"uuid" : ', '"', R_Json_Path, '"}',sep = "")
    mongo_tmp1 <- paste('{"$set":{"inter":',
                        toJSON(list("phase" = phase,
                                    "data" = data_to_use,
                                    "report" = report1_1_tmp,
                                    "acc_success_value" = data_to_use2$acc_success_value),
                               auto_unbox = T),'}}', sep = "")
    db1$update(mongo_tmp, mongo_tmp1)
    
  } else {
    
    db1$insert(list("uuid"=R_Json_Path,
                    "userid"=user_name,
                    "inter"=list("phase" = phase,
                                 "data" = data_to_use,
                                 "report" = report1_1_tmp,
                                 "acc_success_value" = data_to_use2$acc_success_value)),
               na="string",
               auto_unbox = T)  
  
  }
  
  
  
  
  # if (phase==1){
  #   tmp_data <- list(
  #   "time" = as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),  
  #   "phase" = paste(phase_ch,phase,sep=""),  
  #   "report1_mod1" = report1_mod1,
  #   "report1_mod2" = data_to_use2$report1_mod2,
  #   "report2_mod1" = data_to_use2$report2_mod1,
  #   "report2_mod2" = data_to_use2$report2_mod2,
  #   "report2_mod3" = data_to_use2$report2_mod3,
  #   "report2_mod4" = data_to_use2$report2_mod4,
  #   "report2_mod5" = data_to_use2$report2_mod5,
  #   "report3_mod1" = data_to_use2$report3_mod1,
  #   "report3_mod2" = data_to_use2$report3_mod2,
  #   "report4_mod1" = data_to_use2$report4_mod1,
  #   "report5_mod1" = data_to_use2$report5_mod1,
  #   "report5_mod2" = data_to_use2$report5_mod2,
  #   "report5_mod3" = data_to_use2$report5_mod3,
  #   "p1_tmp" = data_to_use,
  #   "p1_report" = report1_1_tmp,
  #   "p1_acc_success_value" = data_to_use2$acc_success_value)
  # } else {
  #   tmp_data <- list(
  #   "time" = as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),
  #   "phase" = paste(phase_ch,phase,sep=""),
  #   "report1_mod1" = report1_mod1,
  #   "report1_mod2" = data_to_use2$report1_mod2,
  #   "report2_mod1" = data_to_use2$report2_mod1,
  #   "report2_mod2" = data_to_use2$report2_mod2,
  #   "report2_mod3" = data_to_use2$report2_mod3,
  #   "report2_mod4" = data_to_use2$report2_mod4,
  #   "report2_mod5" = data_to_use2$report2_mod5,
  #   "report3_mod1" = data_to_use2$report3_mod1,
  #   "report3_mod2" = data_to_use2$report3_mod2,
  #   "report4_mod1" = data_to_use2$report4_mod1,
  #   "report5_mod1" = data_to_use2$report5_mod1,
  #   "report5_mod2" = data_to_use2$report5_mod2,
  #   "report5_mod3" = data_to_use2$report5_mod3)
  #  }
  
  
  ##----------------------------------------------------------------------------
  ##--               write the output results to the mongodb
  ##----------------------------------------------------------------------------
  
  #- create connection, database and collection
  
    mongodb_con <- mongo(collection = "report",
        url = sprintf(
          "mongodb://%s/%s",
          # options()$mongodb$username,
          # options()$mongodb$password,
          options()$mongodb$host,
          "TMIST"))
  
    names_report <- unlist(lapply(to_mongo_tmp, function(x) x$report_name))
    transfer2 <- mongodb_con$find()
    
    if ( R_Json_Path%in%transfer2$uuid ) {
      
      rownn2 <- which(transfer2$uuid==R_Json_Path)
      info <- transfer2[rownn2,]$report[[1]]
      phase_in_mongo <- info$phase
      
      out <-lapply(1:nrow(info), function(x) {
          
          report_name1 <- info$report_name[x]
          
          if (info$phase[x]==phase) {
            chk <- which(names_report==report_name1)
            list("phase"=phase,
                 "report_name"=report_name1,
                 "result"=to_mongo_tmp[[chk]]$result)
          } else {
            list("phase"=info$phase[x],
                 "report_name"= report_name1,
                 "result"=info$result[[x]])
          }
        })
        
      mongo_tmp <- paste('{"uuid" : ', '"', R_Json_Path, '"}',sep = "")
      mongo_tmp2 <- paste('{"$set":{"report":',toJSON(out,auto_unbox = T),'}}', sep = "")
      mongodb_con$update(mongo_tmp, mongo_tmp2)
      
        
    } else {
      
      mongodb_con$insert(to_mongo, auto_unbox = T, na = "string")
      
    }
    
    
    
   

    
  
  # 
  # saveWorkbook(writeDown(tmp_data),
  #              file_path,
  #              overwrite = T)
  

  