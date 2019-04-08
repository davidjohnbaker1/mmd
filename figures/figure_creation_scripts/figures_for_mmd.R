#--------------------------------------------------
## Generate Images
#--------------------------------------------------
# Libraries
library(DiagrammeR)
#--------------------------------------------------

#--------------------------------------------------
# Chapter 1
#--------------------------------------------------

grViz("
     digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]

  # several 'node' statements
  node [shape = box,
        fontname = Helvetica]
  Individual ; Musical ; Cognitive ; Environmental ; Structural ; Experimental

  node [shape = box] // sets as circles
  WMC; Gf; CowanList; Musical_Training; Aural_Training;
  Interval_Structure; Times_Played; Tempo; AuralSkills

  # several 'edge' statements
  AuralSkills -> Individual
  AuralSkills -> Musical
  Individual -> Cognitive
  Individual -> Environmental
  Musical -> Structural
  Musical -> Experimental
  Cognitive -> WMC
  Cognitive -> Gf
  Cognitive -> CowanList
  Environmental -> Musical_Training
  Environmental -> Aural_Training
  Structural -> Interval_Structure
  Experimental -> Times_Played
  Experimental -> Tempo
}

      ")
