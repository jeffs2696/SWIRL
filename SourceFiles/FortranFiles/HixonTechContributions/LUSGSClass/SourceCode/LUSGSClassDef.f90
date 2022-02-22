TYPE LUSGSType
  PRIVATE
  LOGICAL :: isInitialized           = .FALSE.
             
  INTEGER :: numberOfTopologicalDimensions = -1, &
             initialSweepDirection         =  1

END TYPE LUSGSType
