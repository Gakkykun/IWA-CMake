module riverCharacteristics
  use,intrinsic :: iso_fortran_env
  implicit none         
  
  contains
  subroutine riverC(Div_Total, InitialCondition, ALLRiver_Q, ALLRiver_TankV)
      integer, intent(out) :: Div_Total
      real(real64), dimension(22,1), intent(out) :: InitialCondition
      real(real64), intent(out) :: ALLRiver_Q, ALLRiver_TankV

      real(real64) :: River_Q = 0.24, Discharge_Q = 0.1, ALLRiver_n = 0.03, ALLRiver_I = 0.0012, &
            ALLRiver_B = 3.4, ALLRiver_H = 0.2, ALLRiver_L = 300 
      real(real64) ALLRiver_U, ALLRiver_A, ALLRiver_R, ALLRiver_E
      real(real64), dimension(1,22) :: RiverData, DischargeData
      real(real64), dimension(22,1) :: River, Discharge
  
      ALLRiver_Q = River_Q + Discharge_Q
      ALLRiver_A = ALLRiver_B * ALLRiver_H
      ALLRiver_R = ALLRiver_A / (2.0 * ALLRiver_H + ALLRiver_B)
      ALLRiver_U = (1 / ALLRiver_n) * ALLRiver_R ** 0.6666666667 * ALLRiver_I ** 0.5
      ALLRiver_E = 2.0 * sqrt(9.8 * ALLRiver_R * ( (ALLRiver_n ** 2.0 * ALLRiver_U ** 2.0) / ALLRiver_R**(1.3333333333) ) ) * &
                    ALLRiver_H * ( ALLRiver_B / ALLRiver_H )**1.5
      Div_Total = int((ALLRiver_U * ALLRiver_L) / (2.0 * ALLRiver_E) + 1.0)
      ALLRiver_TankV = ALLRiver_B * ALLRiver_H * ALLRiver_L / Div_Total

      if (Div_Total < 10) Then
        Div_Total = 10
      End if

      open(unit=99, file='RivWQ.csv',status='old'); read(99, '()')
      open(unit=98, file='DisWQ.csv',status='old'); read(98, '()')

      read (99,*) RiverData(1, :)         
      read (98,*) DischargeData(1, :)

      River = transpose(RiverData)
      Discharge = transpose(DischargeData)

      ! After mixing
      InitialCondition (:,1) = (River_Q*River(:,1) + Discharge_Q*Discharge(:,1))/ALLRiver_Q

      close (unit=99); close (unit=98)

  end subroutine riverC

end module riverCharacteristics

