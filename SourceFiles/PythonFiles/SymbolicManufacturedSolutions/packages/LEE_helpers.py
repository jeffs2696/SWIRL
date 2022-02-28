def MatrixFormLEE(r,A,M_t,M_x,
        dM_t_dr,dM_x_dr,
        ak,m,i,):

    
    # Creating Matrix
    AA = sp.Matrix(sp.zeros(4,4))

    AA[0, 0] = -i*(ak/A - (m/r)*M_t)
    AA[1, 1] = AA[0, 0]
    AA[2, 2] = AA[0, 0]
    AA[3, 3] = AA[0, 0]

    AA[0, 2] = 0.0
    AA[1, 2] = 0.0
    AA[3, 2] = 0.0
    AA[2, 1] = 0.0
    AA[2, 3] = 0.0
    
    AA[1, 3] = (i*m)/r
    AA[3, 1] = AA[1, 3]
    
    A12 = (-2.0/r)*M_t
    A21 = M_t/r + dM_t_dr + (kappa - 1.0)/(2.0*r)*M_t**3.0
    A31 = dM_x_dr + (kappa - 1.0)/(2.0*r)*M_t**2.0*M_x
    
    AA[0, 1] = A12
    AA[1, 0] = A21
    AA[2, 0] = A31
    
    # note that these terms only have derivative operators , See Eqn 2.52
    
    A41 = (1.0/v_r)*dv_r_dr + 1.0/r + (kappa + 1.0)/(2.0*r)*M_t**2.0
    A14 = (1.0/p)*dp_dr + (kappa - 1.0)/r * M_t**2.0

    AA[3, 0] = A41
    AA[0, 3] = A14

    BB = sp.zeros(4,4)

    BB[0, 0] = M_x
    BB[1, 1] = BB[0, 0]
    BB[2, 2] = BB[0, 0]
    BB[3, 3] = BB[0, 0]

    BB[2, 3] = 1.0
    BB[3, 2] = BB[2,3]

    XX = sp.Matrix(
            [
                [v_r],
                [v_t],
                [v_x],
                [p]
                ]
            )

    Lambda = -i*gamma

    SS = AA*XX - Lambda*BB*XX
    
