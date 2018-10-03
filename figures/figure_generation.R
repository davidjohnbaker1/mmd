#======================================================================================================
# Figure Creation for MMD
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

      # several 'edge' statements
      Aural -> Individual
      Aural -> Musical
      Individual -> Cognitive
      Individual -> Environmental
      Musical -> Structural
      Musical -> Experimental
      }

      ")

#--------------------------------------------------
# Chapter 6
#--------------------------------------------------


