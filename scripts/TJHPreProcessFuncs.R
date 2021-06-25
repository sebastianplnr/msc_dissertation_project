FindGeneralPos <- function(Pos) {
  if(Pos == "Goalkeeper") return("GK")
  if((Pos == "Center Back") | (Pos == "Left Fullback") | (Pos == "Right Fullback")) return("Def")
  if((Pos == "Center Midfielder") | (Pos == "Defensive Midfielder") | (Pos == "Left Midfielder") | (Pos == "Right Midfielder") | (Pos == "Attacking Midfielder")) return("Mid")
  if((Pos == "Center Forward") | (Pos == "Left Winger") | (Pos == "Right Winger")) return("Att")

#Otherwise stop as unknown position  
  stop("Unknown Position")
  return(NA)
}