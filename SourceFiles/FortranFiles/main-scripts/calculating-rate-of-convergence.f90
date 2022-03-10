
    CALL getRateOfConvergence(&
        object            = SoundSpeedMMS_ClassObj , &
        ExpectedRateOfConvergence = ExpectedRateOfConvergenceSoundSpeed   , &
        RateOfConvergence = RateOfConvergence1 , &
        L2Array           = SoundSpeedL2Array)

    CALL getRateOfConvergence(&
        object            = SourceTermMMS_ClassObj, &
        RateOfConvergence = RateOfConvergence2 , &
        L2Array           = S_L2Array)

