PROGRAM MAIN

    USE, INTRINSIC  :: ISO_FORTRAN_ENV
    USE swirlClassObject               ! Runs SWIRL for a given set of parameters
    USE mmsClassObject                 ! Calculates L2Norm, L2Max, and Rate Of Convergence
    USE SourceTermModule               ! Calculated the symbolic terms needed for MMS (used Python)

    IMPLICIT NONE

    ! 2) Start defining the variables  
    CHARACTER(50) :: file_name,dir_name,file_id
    LOGICAL :: &
        debug = .FALSE.

    INTEGER, PARAMETER :: &
        rDef = REAL64   , &
        numberOfIterations = 4

    INTEGER  :: &
        UNIT , & ! for NEWUNIT
        finiteDiffFlag           ,& ! finite difference flag
        numericalIntegrationFlag ,& !  numerical integration flag
        numberOfGridPoints       ,& ! number of points
        azimuthalModeNumber      ,& ! mode order
        i                        ,& ! indexer for do loops
        fac                      ,& ! variable used for doubling grid points
        facCount                    ! counts the outermost do loop

    REAL(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        r                   ,& !radial grid locations
        rOut , &
        vR, vX, vTh, Pr     , &
        speedOfSoundMMS     , &
        SoundSpeedError     , &
        axialMachDataMMS       ,& !M_x
        thetaMachDataMMS     ,& !M_th
        axialMachDataMMSOut       ,& !M_x
        thetaMachDataMMSOut     ,& !M_th
        axialMach_drMMSOut, &
        thetaMach_drMMSOut, &
        SoundSpeedOut , &
        SoundSpeed_drOut, &
        axialMachData       ,& !M_x
        thetaMachData       ,& !M_th
        totalMachData          !M_total = sqrt(M_x^2+M_th^2)

    REAL(KIND = REAL64) ::  &
        r_min                   ,& !radial grid locations
        r_max                 ,& !radial grid locations
        secondOrderSmoother ,& !2nd order smoothing coefficient
        fourthOrderSmoother ,& !4th order smoothing coefficient
        dr                  ,&
        hubToTipRatio       ,&
        SoundSpeedErrorL2

    COMPLEX(KIND=rDef) :: &
        S_L2            ,&
        eigenValueMMS   ,&
        hubAdmittance   ,&
        ductAdmittance  ,&
        frequency       

    COMPLEX(KIND=rDef) , DIMENSION(:), ALLOCATABLE :: S_1, S_2, S_3, S_4, S_MMS, S_error, S_actual, eigenVectorMMS

    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: &
        swirlClassObj, swirlClassObjMMS

    TYPE(mmsClassType) :: SoundSpeedMMS_ClassObj, SourceTermMMS_ClassObj
! Code Starts Here!
    CONTINUE

    ! local variables 
    ! inputs needed for SwirlClassType
    
    r_min                     = 0.30_rDef  
    r_max                     = 1.000_rDef  
    hubToTipRatio             =  r_min/r_max

    azimuthalModeNumber       =  1
    numericalIntegrationFlag  =  1
    finiteDiffFlag            =  1
    secondOrderSmoother       =  0.0_rDef
    fourthOrderSmoother       =  0.0_rDef
    hubAdmittance             = CMPLX(0.0,0,rDef)   
    ductAdmittance            = CMPLX(0.3,0.0,rDef) 
    frequency                 = CMPLX(30,0,rDef)

    eigenValueMMS = CMPLX(100,9,KIND=rDef)

    facCount = 0 ! initializer for fac count 
    DO fac = 1, numberOfIterations
        facCount             = facCount + 1
        numberOfGridPoints   = 5+(2**fac)
        dr                   = (r_max-r_min)/REAL(numberOfGridPoints-1, rDef)

        ALLOCATE(&
            axialMachDataMMSOut(numberOfGridPoints)       ,& !M_x
            thetaMachDataMMSOut(numberOfGridPoints)     ,& !M_th
            axialMach_drMMSOut(numberOfGridPoints), &
            thetaMach_drMMSOut(numberOfGridPoints), &
            SoundSpeedOut(numberOfGridPoints) , &
            SoundSpeed_drOut(numberOfGridPoints), &
            r(numberOfGridPoints)                                            , &
            rOut(numberOfGridPoints) , &
            thetaMachDataMMS(numberOfGridPoints)                                , &
            axialMachDataMMS(numberOfGridPoints)                                , &
            speedOfSoundMMS(numberOfGridPoints), &
            SoundSpeedError(numberOfGridPoints) , &
            vR(numberOfGridPoints), &
            vX(numberOfGridPoints), &
            vTh(numberOfGridPoints), &
            Pr(numberOfGridPoints), &
            S_1(numberOfGridPoints), &
            S_2(numberOfGridPoints), &
            S_3(numberOfGridPoints), &
            S_4(numberOfGridPoints), &
            S_MMS(numberOfGridPoints*4) , &
            S_error(numberOfGridPoints*4) , &
            S_actual(numberOfGridPoints*4) , &
            eigenVectorMMS(numberOfGridPoints*4),&
            thetaMachData(numberOfGridPoints)                                , &
            axialMachData(numberOfGridPoints)                                , &
            totalMachData(numberOfGridPoints))                                

        DO i = 1, numberOfGridPoints

            r(i)             = (r_min+REAL(i-1, rDef)*dr)/r_max
            axialMachData(i) = 0.2_rDef*COS(0.2_rDef*r(i))
            thetaMachData(i) = 0.2_rDef*COS(0.2_rDef*r(i))
        END DO 
            CALL getSoundSpeed(&
                r                  = r                  , &
                SoundSpeedExpected = speedOfSoundMMS , &
                thetaMachData      = thetaMachDataMMS, &
                axialMachData      = axialMachDataMMS      )

            CALL getPerturbationVariables(&
                r     = r    , &
                vR    = vR   , &
                vTh   = vTh   , &
                vX    = vX   , &
                Pr    = Pr      )
            DO i = 1,numberOfGridPoints

                eigenVectorMMS(i) = &
                    CMPLX(vR(i), KIND = rDef)
                eigenVectorMMS(i + numberOfGridPoints) = &
                    CMPLX(vTh(i), KIND = rDef)
                eigenVectorMMS(i + 2*numberOfGridPoints) = &
                    CMPLX(vX(i), KIND = rDef)
                eigenVectorMMS(i + 3*numberOfGridPoints) = &
                    CMPLX(Pr(i), KIND = rDef)
            ENDDO
                CALL getMMSSourceTerms( &
                r     = r                    ,&
                S_1   = S_1                  ,&
                S_2   = S_2                  ,&
                S_3   = S_3                  ,&
                    S_4   = S_4     )


                DO i = 1,numberOfGridPoints

                    S_MMS(i) = S_1(i) 
                    S_MMS(i+numberOfGridPoints) = S_2(i)
                    S_MMS(i+2*numberOfGridPoints) = S_3(i)
                    S_MMS(i+3*numberOfGridPoints) = S_4(i)
                ENDDO
        !Create a swirlClassObj for a given flow
        CALL CreateObject(&
            object        = swirlClassObj(fac)  ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            axialMachData = axialMachData        ,&
            tangentialMachData = thetaMachData        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       )
        
        CALL runSwirlClassMethods(&
            object = swirlClassObj(fac))

        CALL CreateObject(&
            object        = swirlClassObjMMS(fac)  ,&
            azimuthalMode = azimuthalModeNumber  ,&
            np            = numberOfGridPoints   ,&
            sig           = hubToTipRatio        ,&
            axialMachData = axialMachDataMMS        ,&
            tangentialMachData = thetaMachDataMMS        ,&
            ak            = frequency            ,&
            etah          = hubAdmittance        ,&
            etad          = ductAdmittance       ,&
            ifdff         = finiteDiffFlag       )

        CALL runSwirlClassMethods(&
            object = swirlClassObjMMS(fac))

        CALL GetMeanFlowData(&
            object      = swirlClassObjMMS(fac),&
            axialMach   = axialMachDataMMSOut, &
            thetaMach    = thetaMachDataMMSOut, &
            axialMach_dr = axialMach_drMMSOut, &
            thetaMach_dr = thetaMach_drMMSOut, & 
            SoundSpeed = SoundSpeedOut   , &
            SoundSpeed_dr = SoundSpeed_drOut, &
            radialData = rOut)

        CALL FindResidualData(&
            object = swirlClassObjMMS(fac),&
            eigenVector = eigenVectorMMS, &
            eigenValue =  eigenValueMMS, &
            S =  S_actual)


        DO i = 1,numberOfGridPoints
            SoundSpeedError(i) = speedOfSoundMMS(i) - SoundSpeedOut(i)
            S_error(i) = ABS(S_actual(i) - S_MMS(i))
            S_error(i + numberOfGridPoints) = ABS(S_actual(i + numberOfGridPoints) - S_MMS(i + numberOfGridPoints))
            S_error(i + 2*numberOfGridPoints) = ABS(S_actual(i + 2*numberOfGridPoints) - S_MMS(i + 2*numberOfGridPoints))
            S_error(i + 3*numberOfGridPoints) = ABS(S_actual(i + 3*numberOfGridPoints) - S_MMS(i + 3*numberOfGridPoints))
        ENDDO

        CALL getL2Norm(&
            object = SoundSpeedMMS_ClassObj , & 
            L2     = SoundSpeedErrorL2 , &
            dataSet1 = speedOfSoundMMS , &
            dataSet2 = SoundSpeedOut) 
        CALL getL2Norm(&
            object = SourceTermMMS_ClassObj , & 
            L2     = S_L2 , &
            dataSet1 =  S_MMS, &
            dataSet2 = S_actual) 

        CALL DestroyObject(object = swirlClassObjMMS(fac))

        CALL DestroyObject(object = swirlClassObj(fac))


        include 'swirlDataExport.f90'
        DEALLOCATE(&
            axialMachDataMMSOut, &
            thetaMachDataMMSOut, &
            r                     ,&
            rOut,&
            thetaMachData         ,&
            axialMachData         ,&
            thetaMachDataMMS         ,&
            axialMach_drMMSOut         ,&
            thetaMach_drMMSOut         ,&
            SoundSpeedOut         ,&
            SoundSpeedError   , &
            speedOfSoundMMS , &
            SoundSpeed_drOut         ,&
            axialMachDataMMS         ,&
            vR,vTh,vX,Pr, &
            eigenVectorMMS, &
            S_MMS,S_actual, S_error, S_1, S_2, S_3, S_4,&
            totalMachData         )

    END DO


END PROGRAM MAIN
