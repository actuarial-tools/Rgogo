excelPath = "data-raw/Dict.en.xlsx"
dict <- openxlsx::read.xlsx(excelPath)
Dict.en <- dict[order(dict$Key),]
rownames(Dict.en) <- NULL
usethis::use_data(Dict.en, overwrite = TRUE)

