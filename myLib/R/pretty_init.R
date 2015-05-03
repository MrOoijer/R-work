#' @title pretty_init
#' @description Initialize colour palet for pretty_plot routines
#' @param palnr between 1-4; one of 4 available coulour palets.
#' @return NA
#' @examples
#' # pretty_palet(pal.nr=4)

pretty_init<-function(pal.nr=3){
  
  cpalet1= list( 
    rand= rgb(0.05,0.05,0.05)
    , titel=rgb(1,0.8,0)
    , as= rgb(0.5,0.5,0.5)
    , box= rgb(0.3,0.3,0.3)
    , binnen= rgb(0.2,0.2,0.2)
    , lijnkleur=c("red", "blue", "white", "pink2", "pink4")
    , vulkleur= c(rgb(1,0,0,0.15)
                  ,rgb(0,0,1,0.2)
                  ,rgb(1,1,1,0.3)
                  ,rgb(165,42,42,40, max=255))
  )
  
  cpalet2= list( 
    rand="#F3F3F3"
    , titel="#222277"
    , as="#777777"
    , box= "#CCCCCC"
    , binnen="white"
    , lijnkleur=c("red", "blue", "brown", "darkgrey", "lightgrey")
    , vulkleur= c(rgb(1,0,0,0.15)
                  ,rgb(0,0,1,0.2)
                  ,rgb(165,42,42,40, max=255)
                  ,rgb(0.5,0.5,0.5, 0.2)
                  ,rgb(0.5,0.5,0.5, 0.1))
  )
  
  cpalet3= list( 
    rand="#FDDBC7"
    , titel="#67001F"
    , as="#4D4D4D"
    , box= "#BABABA"
    , binnen="#FFFFFF"
    , lijnkleur=c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7")
    , vulkleur= c("#67001F28"
                  , "#B2182B28"
                  , "#D6604D28"
                  , "#F4A58228"
                  , "#FDDBC728")
  )
  
  cpalet4= list( 
    rand="#cad4e8"
    , titel="#8b0000"
    , as="#d84467"
    , box= "#ffccdc"
    , binnen="#fffafa"
    , lijnkleur=c('#8b0000','#b71c39','#d84467','#f07092','#ff9cb8','#ffccdc',
                  '#fffafa',
                  '#cad4e8','#a5abd6','#8481c3','#6359b1','#40319e','#00008b')
    , vulkleur= c('#8b000028','#b71c3928','#d8446728','#f0709228','#ff9cb828','#ffccdc28',
                  '#fffafa28',
                  '#cad4e828','#a5abd628','#8481c328','#6359b128','#40319e28','#00008b28')
  )
  
  if (exists("cpalet")) remove(cpalet, pos=".GlobalEnv")
  if (pal.nr %in% c(1,2,3,4)) cpalet<<- list(cpalet1, cpalet2, cpalet3, cpalet4)[[pal.nr]]
}
