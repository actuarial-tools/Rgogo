New.Const.MaxProjYears <- function() {
   object <- Const(
      value = 120,
      descrip = "Maximum projection years."
   )
   return(object)
}

New.Const.MaxAttAge <- function() {
   object <- Const(
      value = 120L,
      descrip = "Maximum attained age in projection."
   )
   return(object)
}

New.Const.Editor.Tab <- function() {
   object <- Const(
      value = strrep(" ", times = 3),
      descrip = "Tab width."
   )
   return(object)
}

# Global.MaxProjYears <- function(){return(120)}      # Maximum projection years.
# Global.MaxAttAge <- function(){return(120L)}        # Maximum attained age.
# Editor.Tab <- function() {return("   ")}

