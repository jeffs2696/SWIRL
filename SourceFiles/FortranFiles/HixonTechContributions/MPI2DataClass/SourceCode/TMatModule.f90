MODULE TMatModule

  USE ErrorInformationClass

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: GetTMatrix2D, &
            GetTMatrix3D, &
            GetTMatrix4D, &
            GetTMatrix5D

TYPE(CharacterStringType) :: charStringObject, &
                             charStringObjectLast

CHARACTER(LEN=*), PARAMETER :: moduleLocation = 'MODULE TMatModule'

CONTAINS

! how to construct the tMatrix?

! given the iOrder array and deltaI, it will
!  construct the transformation matrix (changing the
!  sign of deltaI if needed to get a RH connection).
!
! The tMatrix is defined as:
!
!  dXiVec(transformed) = [T] (dXiVecLocal)
!   ^
!   |
!    \
!     ----- this is in iOrder ordering, from i/j/k/l local ordering
!

SUBROUTINE GetTMatrix2D(tMatrix,        &
                        iOrderSign,     &
                        enableChecking, &
                        errorInfoObject)

  INTEGER, DIMENSION(:,:), INTENT(INOUT) :: tMatrix
  INTEGER, DIMENSION(:), INTENT(INOUT) :: iOrderSign
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = 'SUBROUTINE GetTMatrix2D'

  INTEGER, DIMENSION(2) :: matSize

  INTEGER :: n,i,j

  INTEGER :: det
  
  CONTINUE ! execution begins here

  IF (enableChecking) THEN

   matSize(1) = SIZE(tMatrix,1)
   matSize(2) = SIZE(tMatrix,2)

   IF (matSize(1) /= matSize(2)) THEN
    WRITE(charStringObjectLast%charString,'(a31,2(1x,i5))')  &
       'ERROR: tMatrix is not square!: ',(matSize(n),n=1,2)
    GO TO 100
   END IF

   IF (matSize(1) /= 2) THEN
    WRITE(charStringObjectLast%charString,'(a31,2(1x,i5))')  &
       'ERROR: tMatrix is not 2x2! :   ',(matSize(n),n=1,2)
    GO TO 100
   END IF

   IF (SIZE(iOrderSign) /= 2) THEN
    WRITE(charStringObjectLast%charString,'(a31,1(1x,i5))')  &
       'ERROR: iOrderSign length is not 2!:',SIZE(iOrderSign)
    GO TO 100
   END IF

  END IF

! the tMatrix is _from_ an i,j system _to_ an iOrderSign system
!
! so, if (for example), iOrderSign is (2,-1) then
!
! the tMatrix is
!
! {dI_out}   | 0  1 | {dI_in}
! {      } = |      | {     }
! {dJ_out}   |-1  0 | {dJ_in}
!
!
  DO i=1,2
   DO j=1,2
    tMatrix(i,j) = 0
   END DO
   tMatrix(i,ABS(iOrderSign(i))) = SIGN(1,iOrderSign(i))
  END DO

  CALL GetMatrixDet2x2(tMatrix, &
                       det)

  IF (det < 0) THEN
   DO n=1,2
    tMatrix(2,n) = -tMatrix(2,n)
   END DO
   iOrderSign(2) = -iOrderSign(2)
  ELSE
   CONTINUE ! all is well
  END IF
 
  RETURN

100 CONTINUE

  charStringObject%charString = 'in '//moduleLocation
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  charStringObject%charString = 'in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObjectLast)

  RETURN
END SUBROUTINE GetTMatrix2D

SUBROUTINE GetTMatrix3D(tMatrix,        &
                        iOrderSign,     &
                        enableChecking, &
                        errorInfoObject)

  INTEGER, DIMENSION(:,:), INTENT(INOUT) :: tMatrix
  INTEGER, DIMENSION(:), INTENT(INOUT) :: iOrderSign
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = 'SUBROUTINE GetTMatrix2D'

  INTEGER, DIMENSION(2) :: matSize

  INTEGER :: n,i,j

  INTEGER :: det
  
  CONTINUE ! execution begins here

  IF (enableChecking) THEN

   matSize(1) = SIZE(tMatrix,1)
   matSize(2) = SIZE(tMatrix,2)

   IF (matSize(1) /= matSize(2)) THEN
    WRITE(charStringObjectLast%charString,'(a31,2(1x,i5))')  &
       'ERROR: tMatrix is not square!: ',(matSize(n),n=1,2)
    GO TO 100
   END IF

   IF (matSize(1) /= 3) THEN
    WRITE(charStringObjectLast%charString,'(a31,2(1x,i5))')  &
       'ERROR: tMatrix is not 3x3! :   ',(matSize(n),n=1,2)
    GO TO 100
   END IF

   IF (SIZE(iOrderSign) /= 3) THEN
    WRITE(charStringObjectLast%charString,'(a31,1(1x,i5))')  &
       'ERROR: iOrderSign length is not 3!:',SIZE(iOrderSign)
    GO TO 100
   END IF

  END IF

! the tMatrix is _from_ an i,j,k system _to_ an iOrderSign system
!
! so, if (for example), iOrderSign is (2,-1,3) then
!
! the tMatrix is
!
! {dI_out}   | 0  1  0 | {dI_in}
! {      } = |         | {     }
! {dJ_out}   |-1  0  0 | {dJ_in}
! {      } = |         | {     }
! {dK_out}   | 0  0  1 | {dK_in}
!
!
  DO i=1,3
   DO j=1,3
    tMatrix(i,j) = 0
   END DO
   tMatrix(i,ABS(iOrderSign(i))) = SIGN(1,iOrderSign(i))
  END DO

  CALL GetMatrixDet3x3(tMatrix, &
                       det)

! reset the k direction if needed

  IF (det < 0) THEN
   DO n=1,3
    tMatrix(3,n) = -tMatrix(3,n)
   END DO
   iOrderSign(3) = -iOrderSign(3)
  ELSE
   CONTINUE ! all is well
  END IF
 
  RETURN

100 CONTINUE

  charStringObject%charString = 'in '//moduleLocation
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  charStringObject%charString = 'in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObjectLast)

  RETURN
END SUBROUTINE GetTMatrix3D

SUBROUTINE GetTMatrix4D(tMatrix,        &
                        iOrderSign,     &
                        enableChecking, &
                        errorInfoObject)

  INTEGER, DIMENSION(:,:), INTENT(INOUT) :: tMatrix
  INTEGER, DIMENSION(:), INTENT(INOUT) :: iOrderSign
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = 'SUBROUTINE GetTMatrix2D'

  INTEGER, DIMENSION(2) :: matSize

  INTEGER :: n,i,j

  INTEGER :: det

  INTEGER, PARAMETER :: numDim = 4
  
  CONTINUE ! execution begins here

  IF (enableChecking) THEN

   matSize(1) = SIZE(tMatrix,1)
   matSize(2) = SIZE(tMatrix,2)

   IF (matSize(1) /= matSize(2)) THEN
    WRITE(charStringObjectLast%charString,'(a31,2(1x,i5))')  &
       'ERROR: tMatrix is not square!: ',(matSize(n),n=1,2)
    GO TO 100
   END IF

   IF (matSize(1) /= numDim) THEN
    WRITE(charStringObjectLast%charString,'(a39,2(1x,i5))')  &
       'ERROR: tMatrix is not correct!:          ',(matSize(n),n=1,2)
    GO TO 100
   END IF

   IF (SIZE(iOrderSign) /= numDim) THEN
    WRITE(charStringObjectLast%charString,'(a39,1(1x,i5))')  &
       'ERROR: iOrderSign length is not correct!:',SIZE(iOrderSign)
    GO TO 100
   END IF

  END IF

! the tMatrix is _from_ an i,j,k system _to_ an iOrderSign system
!
! so, if (for example), iOrderSign is (2,-1,3,4) then
!
! the tMatrix is
!
! {dI_out}   | 0  1  0  0 | {dI_in}
! {      } = |            | {     }
! {dJ_out}   |-1  0  0  0 | {dJ_in}
! {      } = |            | {     }
! {dK_out}   | 0  0  1  0 | {dK_in}
! {      } = |            | {     }
! {dL_out}   | 0  0  0  1 | {dL_in}
!
!
  DO i=1,numDim
   DO j=1,numDim
    tMatrix(i,j) = 0
   END DO
   tMatrix(i,ABS(iOrderSign(i))) = SIGN(1,iOrderSign(i))
  END DO

  CALL GetMatrixDet4x4(tMatrix, &
                       det)

! reset the last direction if needed

  IF (det < 0) THEN
   DO n=1,numDim
    tMatrix(numDim,n) = -tMatrix(numDim,n)
   END DO
   iOrderSign(numDim) = -iOrderSign(numDim)
  ELSE
   CONTINUE ! all is well
  END IF
 
  RETURN

100 CONTINUE

  charStringObject%charString = 'in '//moduleLocation
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  charStringObject%charString = 'in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObjectLast)

  RETURN
END SUBROUTINE GetTMatrix4D

SUBROUTINE GetTMatrix5D(tMatrix,        &
                        iOrderSign,     &
                        enableChecking, &
                        errorInfoObject)

  INTEGER, DIMENSION(:,:), INTENT(INOUT) :: tMatrix
  INTEGER, DIMENSION(:), INTENT(INOUT) :: iOrderSign
  LOGICAL, INTENT(IN) :: enableChecking
  TYPE(ErrorInformationType), INTENT(INOUT) :: errorInfoObject

! local variables

  CHARACTER(LEN=*), PARAMETER :: location = 'SUBROUTINE GetTMatrix5D'

  INTEGER, DIMENSION(2) :: matSize

  INTEGER :: n,i,j

  INTEGER :: det

  INTEGER, PARAMETER :: numDim = 5
  
  CONTINUE ! execution begins here

  IF (enableChecking) THEN

   matSize(1) = SIZE(tMatrix,1)
   matSize(2) = SIZE(tMatrix,2)

   IF (matSize(1) /= matSize(2)) THEN
    WRITE(charStringObjectLast%charString,'(a31,2(1x,i5))')  &
       'ERROR: tMatrix is not square!: ',(matSize(n),n=1,2)
    GO TO 100
   END IF

   IF (matSize(1) /= numDim) THEN
    WRITE(charStringObjectLast%charString,'(a39,2(1x,i5))')  &
       'ERROR: tMatrix is not correct!:          ',(matSize(n),n=1,2)
    GO TO 100
   END IF

   IF (SIZE(iOrderSign) /= numDim) THEN
    WRITE(charStringObjectLast%charString,'(a39,1(1x,i5))')  &
       'ERROR: iOrderSign length is not correct!:',SIZE(iOrderSign)
    GO TO 100
   END IF

  END IF

! the tMatrix is _from_ an i,j,k,l,m system _to_ an iOrderSign system
!
! so, if (for example), iOrderSign is (2,-1,3,4) then
!
! the tMatrix is
!
! {dI_out}   | 0  1  0  0 | {dI_in}
! {      } = |            | {     }
! {dJ_out}   |-1  0  0  0 | {dJ_in}
! {      } = |            | {     }
! {dK_out}   | 0  0  1  0 | {dK_in}
! {      } = |            | {     }
! {dL_out}   | 0  0  0  1 | {dL_in}
!
!
  DO i=1,numDim
   DO j=1,numDim
    tMatrix(i,j) = 0
   END DO
   tMatrix(i,ABS(iOrderSign(i))) = SIGN(1,iOrderSign(i))
  END DO

  CALL GetMatrixDet5x5(tMatrix, &
                       det)

! reset the last direction if needed

  IF (det < 0) THEN
   DO n=1,numDim
    tMatrix(numDim,n) = -tMatrix(numDim,n)
   END DO
   iOrderSign(numDim) = -iOrderSign(numDim)
  ELSE
   CONTINUE ! all is well
  END IF
 
  RETURN

100 CONTINUE

  charStringObject%charString = 'in '//moduleLocation
  CALL SetError(object          = errorInfoObject, &
                errorInfoString = charStringObject)
  charStringObject%charString = 'in '//location
  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObject)

  CALL AddErrorInformation(object          = errorInfoObject, &
                           errorInfoString = charStringObjectLast)

  RETURN
END SUBROUTINE GetTMatrix5D

SUBROUTINE GetMatrixDet2x2(tMatrix, &
                           det)

  INTEGER, DIMENSION(:,:), INTENT(IN) :: tMatrix
  INTEGER, INTENT(OUT) :: det

  det = tMatrix(1,1)*tMatrix(2,2) &
      - tMatrix(2,1)*tMatrix(1,2)

  RETURN
END SUBROUTINE GetMatrixDet2x2

SUBROUTINE GetMatrixDet3x3(tMatrix, &
                           det)

  INTEGER, DIMENSION(:,:), INTENT(IN) :: tMatrix
  INTEGER, INTENT(OUT) :: det

! local variables

  INTEGER :: coFac

  INTEGER, DIMENSION(2,2) :: mat2

  INTEGER :: n,nn,sgn,det2,ii,jj

  sgn = 1
  det = 0
  DO n=1,3
   coFac = tMatrix(1,n)*sgn
   ii = 1    
   IF (n == ii) ii = ii + 1
   jj = ii + 1
   IF (n == jj) jj = jj + 1
   DO nn=1,2
    mat2(nn,1) = tMatrix(nn+1,ii) 
    mat2(nn,2) = tMatrix(nn+1,jj) 
   END DO
   CALL GetMatrixDet2x2(tMatrix = mat2, &
                        det     = det2)
   det = det + coFac*det2
   sgn = -sgn
  END DO

  RETURN
END SUBROUTINE GetMatrixDet3x3

SUBROUTINE GetMatrixDet4x4(tMatrix, &
                           det)

  INTEGER, DIMENSION(:,:), INTENT(IN) :: tMatrix
  INTEGER, INTENT(OUT) :: det

! local variables

  INTEGER :: coFac

  INTEGER, DIMENSION(3,3) :: mat3

  INTEGER :: n,nn,sgn,det3,ii,jj,kk

  sgn = 1
  det = 0
  DO n=1,4
   coFac = tMatrix(1,n)*sgn
   ii = 1    
   IF (n == ii) ii = ii + 1
   jj = ii + 1
   IF (n == jj) jj = jj + 1
   kk = jj + 1
   IF (n == kk) kk = kk + 1
   DO nn=1,3
    mat3(nn,1) = tMatrix(nn+1,ii) 
    mat3(nn,2) = tMatrix(nn+1,jj) 
    mat3(nn,3) = tMatrix(nn+1,kk) 
   END DO
   CALL GetMatrixDet3x3(tMatrix = mat3, &
                        det     = det3)
   det = det + coFac*det3
   sgn = -sgn
  END DO

  RETURN
END SUBROUTINE GetMatrixDet4x4

SUBROUTINE GetMatrixDet5x5(tMatrix, &
                           det)

  INTEGER, DIMENSION(:,:), INTENT(IN) :: tMatrix
  INTEGER, INTENT(OUT) :: det

! local variables

  INTEGER :: coFac

  INTEGER, DIMENSION(4,4) :: mat4

  INTEGER :: n,nn,sgn,det4,ii,jj,kk,ll

  sgn = 1
  det = 0
  DO n=1,5
   coFac = tMatrix(1,n)*sgn
   ii = 1    
   IF (n == ii) ii = ii + 1
   jj = ii + 1
   IF (n == jj) jj = jj + 1
   kk = jj + 1
   IF (n == kk) kk = kk + 1
   ll = kk + 1
   IF (n == ll) ll = ll + 1
   DO nn=1,4
    mat4(nn,1) = tMatrix(nn+1,ii) 
    mat4(nn,2) = tMatrix(nn+1,jj) 
    mat4(nn,3) = tMatrix(nn+1,kk) 
    mat4(nn,4) = tMatrix(nn+1,ll) 
   END DO
   CALL GetMatrixDet4x4(tMatrix = mat4, &
                        det     = det4)
   det = det + coFac*det4
   sgn = -sgn
  END DO

  RETURN
END SUBROUTINE GetMatrixDet5x5

END MODULE TMatModule
