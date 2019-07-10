library(devtools)
a = check()
print(a)
stop()


#this code is to view the errors, warnings, and notes
cat(a$errors)
cat("\n\n\n")
cat(a$warnings)
cat("\n\n\n")
cat(a$notes)
