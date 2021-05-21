MODULE Akima1D

   USE, INTRINSIC :: ISO_FORTRAN_ENV
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: Akima433Interpolation, &
             Akima433CurveFit

INTERFACE Akima433Interpolation
  MODULE PROCEDURE Akima433InterpolationSP
  MODULE PROCEDURE Akima433InterpolationDP
END INTERFACE Akima433Interpolation

INTERFACE Akima433CurveFit
  MODULE PROCEDURE Akima433CurveFitSP
  MODULE PROCEDURE Akima433CurveFitDP
END INTERFACE Akima433CurveFit

CONTAINS

SUBROUTINE Akima433InterpolationDP(inputDataLength,  &
                                   xInputData,       &
                                   yInputData,       &
                                   outputDataLength, &
                                   xOutputData,      &
                                   yOutputData)
 
  INTEGER, INTENT(IN) :: inputDataLength, &
                         outputDataLength
  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)  :: xInputData,  &
                                                  yInputData,  &
                                                  xOutputData
  REAL(KIND=REAL64), DIMENSION(:), INTENT(OUT)  :: yOutputData

! local variables

  INTEGER, PARAMETER :: stdOutput = 6
  
  CALL INTRPLD(stdOutput,        & ! IU
               inputDataLength,  & ! L
               xInputData,       & ! X 
               yInputData,       & ! Y 
               outputDataLength, & ! N
               xOutputData,      & ! U
               yOutputData)        ! V

  RETURN

END SUBROUTINE Akima433InterpolationDP

SUBROUTINE Akima433InterpolationSP(inputDataLength,  &
                                   xInputData,       &
                                   yInputData,       &
                                   outputDataLength, &
                                   xOutputData,      &
                                   yOutputData)
 
  INTEGER, INTENT(IN) :: inputDataLength, &
                         outputDataLength
  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)  :: xInputData,  &
                                                  yInputData,  &
                                                  xOutputData
  REAL(KIND=REAL32), DIMENSION(:), INTENT(OUT)  :: yOutputData

! local variables

  INTEGER, PARAMETER :: stdOutput = 6
  
  CALL INTRPLS(stdOutput,        & ! IU
               inputDataLength,  & ! L
               xInputData,       & ! X 
               yInputData,       & ! Y 
               outputDataLength, & ! N
               xOutputData,      & ! U
               yOutputData)        ! V

  RETURN

END SUBROUTINE Akima433InterpolationSP

SUBROUTINE Akima433CurveFitDP(inputDataLength,           &
                              functionHasMultipleValues, &
                              xInputData,                &
                              yInputData,                &
                              numberOfSubintervals,      &
                              outputDataLength,          &
                              xOutputData,               &
                              yOutputData)

  INTEGER, INTENT(IN) :: inputDataLength,      &
                         numberOfSubintervals, &
                         outputDataLength

  LOGICAL, INTENT(IN) :: functionHasMultipleValues

  REAL(KIND=REAL64), DIMENSION(:), INTENT(IN)  :: xInputData,  &
                                                  yInputData,  &
                                                  xOutputData
  REAL(KIND=REAL64), DIMENSION(:), INTENT(OUT)  :: yOutputData

! local variables

  INTEGER, PARAMETER :: stdOutput = 6

  INTEGER :: curveMode, expectedOutputDataLength

  IF (functionHasMultipleValues) THEN
   curveMode = 2
  ELSE
   curveMode = 1
  END IF

! error check
  
  expectedOutputDataLength = ((inputDataLength-1)*numberOfSubintervals)+1

  IF (outputDataLength /= expectedOutputDataLength) THEN
   WRITE(stdOutput,*) 'Error in Akima433CurveFit: '
   WRITE(stdOutput,*) ' inputDataLength          = ',inputDataLength
   WRITE(stdOutput,*) ' numberOfSubintervals     = ',numberOfSubintervals
   WRITE(stdOutput,*) ' expectedOutputDataLength = ',expectedOutputDataLength
   WRITE(stdOutput,*) ' actualOutputDataLength   = ',outputDataLength
   STOP
  END IF
  
  CALL CRVFITD(stdOutput,            & ! IU
               curveMode,            & ! MD
               inputDataLength,      & ! L
               xInputData,           & ! X 
               yInputData,           & ! Y 
               numberOfSubintervals, & ! M
               outputDataLength,     & ! N
               xOutputData,          & ! U
               yOutputData)            ! V

  RETURN

END SUBROUTINE Akima433CurveFitDP

SUBROUTINE Akima433CurveFitSP(inputDataLength,           &
                              functionHasMultipleValues, &
                              xInputData,                &
                              yInputData,                &
                              numberOfSubintervals,      &
                              outputDataLength,          &
                              xOutputData,               &
                              yOutputData)

  INTEGER, INTENT(IN) :: inputDataLength,      &
                         numberOfSubintervals, &
                         outputDataLength

  LOGICAL, INTENT(IN) :: functionHasMultipleValues

  REAL(KIND=REAL32), DIMENSION(:), INTENT(IN)  :: xInputData,  &
                                                  yInputData,  &
                                                  xOutputData
  REAL(KIND=REAL32), DIMENSION(:), INTENT(OUT)  :: yOutputData

! local variables

  INTEGER, PARAMETER :: stdOutput = 6

  INTEGER :: curveMode, expectedOutputDataLength

  IF (functionHasMultipleValues) THEN
   curveMode = 2
  ELSE
   curveMode = 1
  END IF

! error check
  
  expectedOutputDataLength = ((inputDataLength-1)*numberOfSubintervals)+1

  IF (outputDataLength /= expectedOutputDataLength) THEN
   WRITE(stdOutput,*) 'Error in Akima433CurveFit: '
   WRITE(stdOutput,*) ' inputDataLength          = ',inputDataLength
   WRITE(stdOutput,*) ' numberOfSubintervals     = ',numberOfSubintervals
   WRITE(stdOutput,*) ' expectedOutputDataLength = ',expectedOutputDataLength
   WRITE(stdOutput,*) ' actualOutputDataLength   = ',outputDataLength
   STOP
  END IF
  
  CALL CRVFITS(stdOutput,            & ! IU
               curveMode,            & ! MD
               inputDataLength,      & ! L
               xInputData,           & ! X 
               yInputData,           & ! Y 
               numberOfSubintervals, & ! M
               outputDataLength,     & ! N
               xOutputData,          & ! U
               yOutputData)            ! V

  RETURN

END SUBROUTINE Akima433CurveFitSP

END MODULE Akima1D
