.Translate <- function(s, lang = "en", strCase = "title") {
   ascii <- utf8ToInt(s)
   wordList <- list()
   A <- utf8ToInt("A")
   Z <- utf8ToInt("Z")
   a <- utf8ToInt("a")
   z <- utf8ToInt("z")
   for (i in 1:length(ascii)) {
      if (i == 1) {
         wordList <- c(wordList, list(ascii[i]))
      } else {
         if (ascii[i] >= A & ascii[i] <= Z) {
            if (!(ascii[i - 1] >= A & ascii[i - 1] <= Z)) {    
               wordList <- c(wordList, list(ascii[i]))
            } else {
               if (i < length(ascii) & ascii[i + 1] >= a & ascii[i + 1] <= z) {
                  wordList <- c(wordList, list(ascii[i]))
               } else {
                  wordList[[length(wordList)]] <- c(wordList[[length(wordList)]], ascii[i])
               }
            }
         } else {
            wordList[[length(wordList)]] <- c(wordList[[length(wordList)]], ascii[i])
         }
      }
   }
   l <- lapply(wordList, function(w) {gsub("[._]", "", intToUtf8(w))})
   dict <- eval(expr = parse(text = paste0("Dict.", lang)))
   ss <- lapply(
      X = l, 
      FUN = function(key, strCase) {
         value <- dict$Value[which(dict$Key == tolower(key))]
         if (length(value) == 0) {
            value <- key
         } else {
            value <- switch (
               strCase,
               lower = tolower(value),
               upper = toupper(value),
               title = tools::toTitleCase(value),
               stop("Invalid value for argument 'case'")
            )
         }
         return(value)
      },
      strCase
   )
   output <- paste0(ss, collapse = " ")
   return(output)
}
   
Translate <- function(v, lang = "en", strCase = "title") {
   output <- lapply(
      X = v,
      FUN = function(s, lang, strCase) {
         return(.Translate(s, lang, strCase))
      }, lang, strCase
   )
   return(unlist(output))
}   
  
   
