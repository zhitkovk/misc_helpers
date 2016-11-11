# Numeric to float with bicycle created from ncars. Better to do this through regexp

d <- openxlsx::read.xlsx("data.xlsx", sheet = 1)
d[,251] <- gsub("[[:space:]]", "", d[,251])
d[,251] <- gsub("[[:punct:]]", "", d[,251])


d[,251] <- as.character(as.numeric(d[,251]))

d[,251] <- ifelse(nchar(d[,251]) > 2,
                  gsub('^([0-9]{2})', '\\1\\,\\2', d[,251]),
                  d[,251])

summary(as.numeric(d[,251]))
xlsx::write.xlsx(d, "d.xlsx")

