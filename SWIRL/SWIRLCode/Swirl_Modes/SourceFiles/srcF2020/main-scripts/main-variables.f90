    TYPE(SwirlClassType) , DIMENSION(numberOfIterations) :: &
        swirlClassObj

    TYPE(mmsClassType)  :: &
        SoundSpeedMMS_ClassObj , &
        SourceTermMMS_ClassObj

    INTEGER  :: &
        UNIT                ,& ! for NEWUNIT
        finiteDiffFlag      ,& ! finite difference flag
        azimuthalModeNumber ,& ! mode order
        numberOfGridPoints  ,& ! number of points
        i ,&!j               ,& ! indexer for do loops
        i1,&!j1            ,& ! indexer for do loops
        i2,&!j2            ,& ! indexer for do loops
        i3,&!j3            ,& ! indexer for do loops
        fac                 ,& ! variable used for doubling grid points
        eigenIndex          ,&
        facCount               ! counts the outermost do loop

    LOGICAL :: debug = .FALSE.!.TRUE.

    COMPLEX(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        k                                            , &
        S_eig                                        , &
        S_actual                                     , &
        S_1                                          , &
        S_2                                          , &
        S_3                                          , &
        S_4                                          , &
        S_A11                                        , &   
        S_A12                                        , &   
        S_A13                                        , &   
        S_A14                                        , &   
        S_A21                                        , &   
        S_A22                                        , &   
        S_A23                                        , &   
        S_A24                                        , &   
        S_A31                                        , &   
        S_A32                                        , &   
        S_A33                                        , &   
        S_A34                                        , &   
        S_A41                                        , &   
        S_A42                                        , &   
        S_A43                                        , &   
        S_A44                                        , &   
        S_B11                                        , &   
        S_B12                                        , &   
        S_B13                                        , &   
        S_B14                                        , &   
        S_B21                                        , &   
        S_B22                                        , &   
        S_B23                                        , &   
        S_B24                                        , &   
        S_B31                                        , &   
        S_B32                                        , &   
        S_B33                                        , &   
        S_B34                                        , &   
        S_B41                                        , &   
        S_B42                                        , &   
        S_B43                                        , &   
        S_B44                                        , &   
        S_Expected                                   , &
        S_error                                      , &
        S_L2Array                                    , &
        eigenVector                                  , &
        eigenVectorMMS

    COMPLEX(KIND = rDef) :: &
        frequency          ,& !non-dimensional frequency
        hubAdmittance      ,& !Liner Admittance At the Hub
        ductAdmittance     ,&
        ci                 ,&
        S_L2               ,& 
        axialWavenumberMMS              

     COMPLEX(KIND = rDef),DIMENSION(:), ALLOCATABLE :: &
     !     S_A_expected , &
     !     S_B_expected , &
         S_A_actual   , &
         S_B_actual  


    REAL(KIND = rDef), DIMENSION(:), ALLOCATABLE :: &
        r                   ,& !radial grid locations
        axialMachData       ,& !M_x
        thetaMachData       ,& !M_th
        totalMachData       ,& !M_total = sqrt(M_x^2+M_th^2)
        SoundSpeedExpected  ,& !Based on Eqn 2.6 in Kousen's paper
        rOut                ,& !radial grid after it leaves swirlClassObj
        axialMachDataOut    ,& !M_x         after it leaves swirlClassObj
        thetaMachDataOut    ,& !M_th        after it leaves swirlClassObj 
        SoundSpeedOut       ,& !Sound Speed after it leaves swirlClassObj 
        axialMachData_dr_Out,& !dM_x/dr 
        thetaMachData_dr_Out,& !dM_th/dr
        SoundSpeed_dr_Out   ,& !dA/dr
        SoundSpeedError     ,& !eps_A
        SoundSpeedL2Array   ,& !array of L2norm (eps_A)
    ! Perturbation variables
        vR                  ,& !radial     velocity  
        vT                  ,& !tangential velocity
        vX                  ,& !axial velocity
        Pr                  ,& !pressure
        RateOfConvergence1  ,&
        RateOfConvergence2 

    REAL(KIND = REAL64) ::  &
        gam                      ,&
        gm1                      ,&
        secondOrderSmoother ,& !2nd order smoothing coefficient
        fourthOrderSmoother ,& !4th order smoothing coefficient
        boundingConstant    ,&
        dr                  ,&
        hubToTipRatio       ,&
        SoundSpeedErrorL2


    REAL(KIND = rDef), PARAMETER ::&
        r_min  = 0.20_rDef  ,&
        r_max  = 1.000_rDef  ,&
        r2     = 0.50_rDef  ,&
        r3     = 0.200_rDef

    CHARACTER(50) :: &
        dir_name

    CHARACTER(50):: &
        FORMAT_MEAN_FLOW         , &
        FORMAT_PERTURB_VARS      , &
        FORMAT_PERTURB_HEADER    , &
        FORMAT_MEAN_FLOW_HEADER  , &
        FORMAT_SOURCE_TERMS      , &
        FORMAT_SOURCE_TERMS_HEADER , &
        FORMAT_L2                , &
        FORMAT_L2_HEADER         , &
        FORMAT_ERROR             , &
        FORMAT_ERROR_HEADER      , &
        FORMAT_ROC               , &     
        FORMAT_ROC_HEADER       

    CHARACTER(10):: file_id

