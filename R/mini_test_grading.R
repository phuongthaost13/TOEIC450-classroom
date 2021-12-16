library(googlesheets4)
library(dplyr)
library(tidytext)

SS  <- 'https://docs.google.com/spreadsheets/d/1pkfJPonq2RkQUCRkfnAglYqW0D5q8aOJz8X49BAYHnI/edit#gid=0'

# ss
student_ss <- read_sheet(ss = SS,
                  sheet = "student_ss")
mini_test_ss <- read_sheet(ss = SS,
                        sheet = "mini_test")
# làm đề 
mini_test <- read_sheet(ss = mini_test_ss$ss,
                        sheet = as.character(Sys.Date()))
question <- mini_test %>% 
  select(Question, Type) %>% 
  mutate(Date = Sys.Date(),
         Answer = NA)

sapply(student_ss$ss, function(x) write_sheet(ss = x, data=question, sheet = as.character(Sys.Date())))

# sending the result (điểm + đáp án sai để chép phạt)
# điểm
mark_here <- function(SS) {
  all.answer <- read_sheet(ss=SS,sheet = as.character(Sys.Date())) %>% 
    tidyr::replace_na(list(Answer = "unknown"))
  true.answer <- all.answer %>% 
    inner_join(mini_test)
  mark <- data.frame(Date = (Sys.Date()),
                     Mark = paste(as.character(nrow(true.answer)),as.character(nrow(all.answer)),
                                  sep = "/"),
                     GPA = nrow(true.answer)/nrow(all.answer))
  sheet_append(ss=SS, mark, sheet = "mark")
  if (mark$GPA == 0) {
    print("pai") 
  } else {
    wrong.answer <- anti_join(all.answer,true.answer) %>% 
      select(Question, Answer, Date)
    sheet_append(ss=SS, wrong.answer, sheet= "wrong-word")
  }
}

sapply(student_ss$ss, mark_here)
#mark_here(student_ss$ss[3])  
