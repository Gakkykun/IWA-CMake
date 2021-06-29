program IWA
    use,intrinsic :: iso_fortran_env
    use riverCharacteristics
    use reactionCalc
    implicit none

    real(real64) :: Simulation_duration = 1.0, deltaT = 0.001  ![d]
    real(real64) ALLRiver_Q, ALLRiver_TankV
    integer Time_Total, Div_Total
    integer Loc_Pointer, Time_Pointer, Item_Pointer, Runge_Pointer
    real(real64), allocatable, dimension(:,:,:) :: ALLRiver  
    real(real64), dimension(22,1) ::  InitialCondition                    
    real(real64), dimension(22,1) :: TempPointer, Tempval_Inf, ReactionTerm   
    real(real64), dimension(22,4)  :: RungeK
                        
    Time_Total = int(Simulation_duration/deltaT)
    call riverC(Div_Total, InitialCondition, ALLRiver_Q, ALLRiver_TankV)
    allocate (ALLRiver(22, Time_Total, Div_Total))

    ! Load initial conditions to ALLRiver
    do Item_Pointer = 1, 22
        ALLRiver(Item_Pointer,1,1:Div_Total) = InitialCondition(Item_Pointer, 1)
    end do

    open(unit=10, file='Output.csv',status='replace')

    ! Header info
    write(10, *) 'Time, location, Ss, SI, SNH4, SNH3, SNO2, SNO3, SHPO4, SH2PO4, SO2, SCO2, &
                   & SHCO3, SCO3, SH, SOH, SCa, XH, XN1, XN2, XALG, XCON, XS, XI'

    ! Loop calculation
    do Time_Pointer = 2, Time_Total
        do Loc_Pointer = 1, Div_Total  
            do Runge_Pointer = 1, 4 
                ! Set concentrations in a reservoir depending on a rank in the Runge-Kutta method                   
                if (Runge_Pointer == 1) then
                    TempPointer(:, 1) = ALLRiver(:,Time_Pointer-1,Loc_Pointer)
                else if (Runge_Pointer == 2) then
                    TempPointer(:, 1) = ALLRiver(:,Time_Pointer-1,Loc_Pointer) + (RungeK(:,1)*deltaT/2.0)
                else if (Runge_Pointer == 3) then
                    TempPointer(:, 1) = ALLRiver(:,Time_Pointer-1,Loc_Pointer) + (RungeK(:,2)*deltaT/2.0)
                else if (Runge_Pointer == 4) then
                    TempPointer(:, 1) = ALLRiver(:,Time_Pointer-1,Loc_Pointer) + RungeK(:,3)*deltaT
                end if

                ! Set influent concentrations
                if (Loc_Pointer == 1) then
                    Tempval_Inf(:, 1) = ALLRiver(:, 1, Loc_Pointer)                                                    
                else    
                    Tempval_Inf(:, 1) = ALLRiver(:, Time_Pointer-1, Loc_Pointer-1)                                                    
                end if

                ! retrieve a reaction term
                call reaction(TempPointer, ReactionTerm)

                ! Calculation of 'ks'(slopes or derivatives) in the Runge-Kutta method
                RungeK(:, Runge_Pointer) = (ALLRiver_Q / ALLRiver_TankV) * &
                    (Tempval_Inf(:,1) - TempPointer(:,1)) + ReactionTerm(:,1)
            end do !Runge_Pointer
                             
            ! Update of concentrations
            ALLRiver(:,Time_Pointer,Loc_Pointer) = ALLRiver(:,Time_Pointer-1,Loc_Pointer) &
                + (RungeK(:,1) + RungeK(:,2)*2.0 + RungeK(:,3)*2.0 + RungeK(:,4)) * deltaT/6.0  
                                                                                        
            ! Writing data in a csv file                
            if (mod(Time_Pointer, 100).eq. 0) then
                write(10, 200) Loc_Pointer, Time_Pointer*deltaT, ALLRiver(:, Time_Pointer,Loc_Pointer)
            end if 

        end do !Loc_counter
    end do  !Time_counter
    
    close (unit=10)

200 format(I8, ',', 22(F10.4, ','), F10.4)    
                    
end program IWA