Data_Create <- function(N = 100) {
  data.frame(
  Height_Score = rnorm(N,5,1),
  Weight_Score = rnorm(N,5,1),
  Agility_Score =
    ifelse( "Height_Score" >= 6, 2,
                        ifelse( "Height_Score" >= 5, 3,
                                ifelse( "Height_Score" >= 4, 4,
                                        ifelse("Height_Score" >= 3, 5 , 6))))
  )

}
