(idyom-db:import-data 
	  :krn
	  "~/Desktop/projects/mmd/corpus/symbolic/krn/"
	  "Total corpus" 
	  1)

 (idyom:idyom 1 '(cpitch) '(cpint cpintfref)
		      :output-path  "~/Desktop/projects/mmd/corpus/symbolic/idyom"
		      :overwrite t
		      :separator #\tab
		      :detail 3)
