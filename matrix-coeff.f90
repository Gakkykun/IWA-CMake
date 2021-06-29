module matrixCoeff
use,intrinsic :: iso_fortran_env
implicit none

contains
subroutine matrix(Chemsp)
  real(real64), dimension(28,22), intent(out) :: Chemsp

  Chemsp(1,1)=-1.9; Chemsp(1,2)=0; Chemsp(1,3)=-0.012; Chemsp(1,4)=0; Chemsp(1,5)=0; Chemsp(1,6)=0; Chemsp(1,7)=-0.0083 
  Chemsp(1,8)=0; Chemsp(1,9)=-0.85; Chemsp(1,10)=0; Chemsp(1,11)=0.27; Chemsp(1,12)=0; Chemsp(1,13)=0.023; Chemsp(1,14)=0
  Chemsp(1,15)= 0; Chemsp(1,16)=1.0; Chemsp(1,17)=0; Chemsp(1,18)=0; Chemsp(1,19)=0; Chemsp(1,20)=0; Chemsp(1,21)=0; 
  Chemsp(1,22)=0

  Chemsp(2,1)=-1.9; Chemsp(2,2)=0; Chemsp(2,3)=0; Chemsp(2,4)=0; Chemsp(2,5)=0; Chemsp(2,6)=-0.012; Chemsp(2,7)=-0.0083; 
  Chemsp(2,8)=0; Chemsp(2,9)=-0.8; Chemsp(2,10)=0; Chemsp(2,11)=0.27; Chemsp(2,12)=0; Chemsp(2,13)=0.021; Chemsp(2,14)=0; 
  Chemsp(2,15)=0; Chemsp(2,16)=1.0; Chemsp(2,17)=0; Chemsp(2,18)=0; Chemsp(2,19)=0; Chemsp(2,20)=0; Chemsp(2,21)=0 
  Chemsp(2,22)=0

  Chemsp(3,1)=0; Chemsp(3,2)=0; Chemsp(3,3)=0.071; Chemsp(3,4)=0; Chemsp(3,5)=0; Chemsp(3,6)=0; Chemsp(3,17)=0.017; 
  Chemsp(3,8)=0; Chemsp(3,9)=-0.77; Chemsp(3,10)=0; Chemsp(3,11)=0.25; Chemsp(3,12)=0; Chemsp(3,13)=0.017; Chemsp(3,14)=0; 
  Chemsp(3,15)=0; Chemsp(3,16)=-1.0; Chemsp(3,17)=0; Chemsp(3,18)=0; Chemsp(3,19)=0; Chemsp(3,20)=0; Chemsp(3,21)=0; 
  Chemsp(3,22)=0.23

  Chemsp(4,1)=-2.2; Chemsp(4,2)=0; Chemsp(4,3)=0; Chemsp(4,4)=0; Chemsp(4,5)=1.1; Chemsp(4,6)=-1.1; Chemsp(4,7)=-0.0062 
  Chemsp(4,8)=0; Chemsp(4,9)=0; Chemsp(4,10)=0; Chemsp(4,11)=0.39; Chemsp(4,12)=0; Chemsp(4,13)=0.032; Chemsp(4,14)=0; 
  Chemsp(4,15)=0; Chemsp(4,16)=1.0; Chemsp(4,17)=0; Chemsp(4,18)=0; Chemsp(4,19)=0; Chemsp(4,20)=0; Chemsp(4,21)=0
  Chemsp(4,22)=0

  Chemsp(5,1)=-3.7; Chemsp(5,2)=0; Chemsp(5,3)=0; Chemsp(5,4)=0; Chemsp(5,5)=-1.6; Chemsp(5,6)=0; Chemsp(5,7)=0.0021; 
  Chemsp(5,8)=0; Chemsp(5,9)=0; Chemsp(5,10)=0; Chemsp(5,11)=0.86; Chemsp(5,12)=0; Chemsp(5,13)=-0.045; Chemsp(5,14)=0; 
  Chemsp(5,15)=0; Chemsp(5,16)=1.0; Chemsp(5,17)=0; Chemsp(5,18)=0; Chemsp(5,19)=0; Chemsp(5,20)=0; Chemsp(5,21)=0; 
  Chemsp(5,22)=0

  Chemsp(6,1)=0; Chemsp(6,2)=0; Chemsp(6,3)=0.071; Chemsp(6,4)=0; Chemsp(6,5)=0; Chemsp(6,6)=-0.27; Chemsp(6,7)=0.017; 
  Chemsp(6,8)=0; Chemsp(6,9)=0; Chemsp(6,10)=0; Chemsp(6,11)=0.25; Chemsp(6,12)=0; Chemsp(6,13)=0.0025; Chemsp(6,14)=0; 
  Chemsp(6,15)=0; Chemsp(6,16)=-1.0; Chemsp(6,17)=0; Chemsp(6,18)=0; Chemsp(6,19)=0; Chemsp(6,20)=0; Chemsp(6,21)=0; 
  Chemsp(6,22)=0.23

  Chemsp(7,1)=0; Chemsp(7,2)=0; Chemsp(7,3)=-4.8; Chemsp(7,4)=0; Chemsp(7,5)=4.7; Chemsp(7,6)=0; Chemsp(7,7)=-0.019; 
  Chemsp(7,8)=0; Chemsp(7,9)=-15; Chemsp(7,10)=0; Chemsp(7,11)=-0.32; Chemsp(7,12)=0; Chemsp(7,13)=0.65; Chemsp(7,14)=0; 
  Chemsp(7,15)=0; Chemsp(7,16)=0; Chemsp(7,17)=1.0; Chemsp(7,18)=0; Chemsp(7,19)=0; Chemsp(7,20)=0; Chemsp(7,21)=0
  Chemsp(7,22)=0

  Chemsp(8,1)=0; Chemsp(8,2)=0; Chemsp(8,3)=0.071; Chemsp(8,4)=0; Chemsp(8,5)=0; Chemsp(8,6)=0; Chemsp(8,7)=0.017; 
  Chemsp(8,8)=0; Chemsp(8,9)=-0.77; Chemsp(8,10)=0; Chemsp(8,11)=0.25; Chemsp(8,12)=0; Chemsp(8,13)=0.017; Chemsp(8,14)=0; 
  Chemsp(8,15)=0; Chemsp(8,16)=0; Chemsp(8,17)=-1.0; Chemsp(8,18)=0; Chemsp(8,19)=0; Chemsp(8,20)=0; Chemsp(8,21)=0; 
  Chemsp(8,22)=0.23

  Chemsp(9,1)=0; Chemsp(9,2)=0; Chemsp(9,3)=0; Chemsp(9,4)=0; Chemsp(9,5)=-21.0; Chemsp(9,6)=21.0; Chemsp(9,7)=-0.019; 
  Chemsp(9,8)=0; Chemsp(9,9)=-22; Chemsp(9,10)=0; Chemsp(9,11)=-0.32; Chemsp(9,12)=0; Chemsp(9,13)=-0.033; Chemsp(9,14)=0; 
  Chemsp(9,15)=0; Chemsp(9,16)=0; Chemsp(9,17)=0; Chemsp(9,18)=1.0; Chemsp(9,19)=0; Chemsp(9,20)=0; Chemsp(9,21)=0; 
  Chemsp(9,22)=0

  Chemsp(10,1)=0; Chemsp(10,2)=0; Chemsp(10,3)=0.071; Chemsp(10,4)=0; Chemsp(10,5)=0; Chemsp(10,6)=0; Chemsp(10,7)=0.017; 
  Chemsp(10,8)=0; Chemsp(10,9)=-0.77; Chemsp(10,10)=0; Chemsp(10,11)=0.25; Chemsp(10,12)=0; Chemsp(10,13)=0.017; Chemsp(10,14)=0; 
  Chemsp(10,15)=0; Chemsp(10,16)=0; Chemsp(10,17)=0; Chemsp(10,18)=-1.0; Chemsp(10,19)=0; Chemsp(10,20)=0; Chemsp(10,21)=0; 
  Chemsp(10,22)=0.23

  Chemsp(11,1)=0; Chemsp(11,2)=0; Chemsp(11,3)=-0.065; Chemsp(11,4)=0; Chemsp(11,5)=0; Chemsp(11,6)=0; Chemsp(11,7)=-0.011; 
  Chemsp(11,8)=0; Chemsp(11,9)=1.0; Chemsp(11,10)=0; Chemsp(11,11)=-0.39; Chemsp(11,12)=0; Chemsp(11,13)=-0.028; Chemsp(11,14)=0; 
  Chemsp(11,15)=0; Chemsp(11,16)=0; Chemsp(11,17)=0; Chemsp(11,18)=0; Chemsp(11,19)=1.0; Chemsp(11,20)=0; Chemsp(11,21)=0; 
  Chemsp(11,22)=0

  Chemsp(12,2)=0; Chemsp(12,2)=0; Chemsp(12,3)=0; Chemsp(12,4)=0; Chemsp(12,5)=0; Chemsp(12,6)=-0.065; Chemsp(12,7)=-0.011; 
  Chemsp(12,8)=0; Chemsp(12,9)=1.3; Chemsp(12,10)=0; Chemsp(12,11)=-0.39; Chemsp(12,12)=0; Chemsp(12,13)=-0.038; Chemsp(12,14)=0; 
  Chemsp(12,15)=0; Chemsp(12,16)=0; Chemsp(12,17)=0; Chemsp(12,18)=0; Chemsp(12,19)=1.0; Chemsp(12,20)=0; Chemsp(12,21)=0; 
  Chemsp(12,22)=0

  Chemsp(13,1)=0; Chemsp(13,2)=0; Chemsp(13,3)=0.058; Chemsp(13,4)=0; Chemsp(13,5)=0; Chemsp(13,6)=0; Chemsp(13,7)=0.0086; 
  Chemsp(13,8)=0; Chemsp(13,9)=-0.6; Chemsp(13,10)=0; Chemsp(13,11)=0.26; Chemsp(13,12)=0; Chemsp(13,13)=0.018; Chemsp(13,14)=0; 
  Chemsp(13,15)=0; Chemsp(13,16)=0; Chemsp(13,17)=0; Chemsp(13,18)=0; Chemsp(13,19)=-1.0; Chemsp(13,20)=0; Chemsp(13,21)=0; 
  Chemsp(13,22)=0.4

  Chemsp(14,1)=0; Chemsp(14,2)=0; Chemsp(14,3)=0.029; Chemsp(14,4)=0; Chemsp(14,5)=0; Chemsp(14,6)=0; Chemsp(14,7)=0.0041; 
  Chemsp(14,8)=0; Chemsp(14,9)=0.2; Chemsp(14,10)=0; Chemsp(14,11)=0.0018; Chemsp(14,12)=0; Chemsp(14,13)=0.0016; Chemsp(14,14)=0; 
  Chemsp(14,15)=0; Chemsp(14,16)=0; Chemsp(14,17)=0; Chemsp(14,18)=0; Chemsp(14,19)=-1.0; Chemsp(14,20)=0; Chemsp(14,21)=0.95; 
  Chemsp(14,22)=0.25

  Chemsp(15,1)=0; Chemsp(15,2)=0; Chemsp(15,3)=0.13; Chemsp(15,4)=0; Chemsp(15,5)=0; Chemsp(15,6)=0; Chemsp(15,7)=0.022; 
  Chemsp(15,8)=0; Chemsp(15,9)=-0.15; Chemsp(15,10)=0; Chemsp(15,11)=0.32; Chemsp(15,12)=0; Chemsp(15,13)=0.019; Chemsp(15,14)=0; 
  Chemsp(15,15)=0; Chemsp(15,16)=0; Chemsp(15,17)=0; Chemsp(15,18)=0; Chemsp(15,19)=-5; Chemsp(15,20)=1.0; Chemsp(15,21)=3.8; 
  Chemsp(15,22)=0

  Chemsp(16,1)=0; Chemsp(16,2)=0; Chemsp(16,3)=0.13; Chemsp(16,4)=0; Chemsp(16,5)=0; Chemsp(16,6)=0; Chemsp(16,7)=0.022; 
  Chemsp(16,8)=0; Chemsp(16,9)=-4.8; Chemsp(16,10)=0; Chemsp(16,11)=1.5; Chemsp(16,12)=0; Chemsp(16,13)=0.11; Chemsp(16,14)=0; 
  Chemsp(16,15)=0; Chemsp(16,16)=0; Chemsp(16,17)=0; Chemsp(16,18)=0; Chemsp(16,19)=0; Chemsp(16,20)=1.0; Chemsp(16,21)=-5.8; 
  Chemsp(16,22)=0

  Chemsp(17,1)=0; Chemsp(17,2)=0; Chemsp(17,3)=0.45; Chemsp(17,4)=0; Chemsp(17,5)=0; Chemsp(17,6)=0; Chemsp(17,7)=0.13; 
  Chemsp(17,8)=0; Chemsp(17,9)=-3.8; Chemsp(17,10)=0; Chemsp(17,11)=1.2; Chemsp(17,12)=0; Chemsp(17,13)=0.075; Chemsp(17,14)=0; 
  Chemsp(17,15)=0; Chemsp(17,16)=-8.7; Chemsp(17,17)=0; Chemsp(17,18)=0; Chemsp(17,19)=0; Chemsp(17,20)=1.0; Chemsp(17,21)=3.8; 
  Chemsp(17,22)=0

  Chemsp(18,1)=0; Chemsp(18,2)=0; Chemsp(18,3)=0.45; Chemsp(18,4)=0; Chemsp(18,5)=0; Chemsp(18,6)=0; Chemsp(18,7)=0.13; 
  Chemsp(18,8)=0; Chemsp(18,9)=-3.8; Chemsp(18,10)=0; Chemsp(18,11)=1.2; Chemsp(18,12)=0; Chemsp(18,13)=0.075; Chemsp(18,14)=0; 
  Chemsp(18,15)=0; Chemsp(18,16)=0; Chemsp(18,17)=-8.7; Chemsp(18,18)=0; Chemsp(18,19)=0; Chemsp(18,20)=1.0; Chemsp(18,21)=3.8; 
  Chemsp(18,22)=0

  Chemsp(19,1)=0; Chemsp(19,2)=0; Chemsp(19,3)=0.45; Chemsp(19,4)=0; Chemsp(19,5)=0; Chemsp(19,6)=0; Chemsp(19,7)=0.13; 
  Chemsp(19,8)=0; Chemsp(19,9)=-3.8; Chemsp(19,10)=0; Chemsp(19,11)=1.2; Chemsp(19,12)=0; Chemsp(19,13)=0.075; Chemsp(19,14)=0; 
  Chemsp(19,15)=0; Chemsp(19,16)=0; Chemsp(19,17)=0; Chemsp(19,18)=-8.7; Chemsp(19,19)=0; Chemsp(19,20)=1.0; Chemsp(19,21)=3.8; 
  Chemsp(19,22)=0

  Chemsp(20,1)=0; Chemsp(20,2)=0; Chemsp(20,3)=0.058; Chemsp(20,4)=0; Chemsp(20,5)=0; Chemsp(20,6)=0; Chemsp(20,7)=0.0086; 
  Chemsp(20,8)=0; Chemsp(20,9)=-0.6; Chemsp(20,10)=0; Chemsp(20,11)=0.26; Chemsp(20,12)=0; Chemsp(20,13)=0.018; Chemsp(20,14)=0; 
  Chemsp(20,15)=0; Chemsp(20,16)=0; Chemsp(20,17)=0; Chemsp(20,18)=0; Chemsp(20,19)=0; Chemsp(20,20)=-1.0; Chemsp(20,21)=0; 
  Chemsp(20,22)=0.4

  Chemsp(21,1)=0; Chemsp(21,2)=0; Chemsp(21,3)=0.029; Chemsp(21,4)=0; Chemsp(21,5)=0; Chemsp(21,6)=0; Chemsp(21,7)=0.0041; 
  Chemsp(21,8)=0; Chemsp(21,9)=0.2; Chemsp(21,10)=0; Chemsp(21,11)=0.0018; Chemsp(21,12)=0; Chemsp(21,13)=0.0016; Chemsp(21,14)=0; 
  Chemsp(21,15)=0; Chemsp(21,16)=0; Chemsp(21,17)=0; Chemsp(21,18)=0; Chemsp(21,19)=0; Chemsp(21,20)=-1.0; Chemsp(21,21)=0.95; 
  Chemsp(21,22)=0.25

  Chemsp(22,1)=1.0; Chemsp(22,2)=0; Chemsp(22,3)=0; Chemsp(22,4)=0; Chemsp(22,5)=0; Chemsp(22,6)=0; Chemsp(22,7)=0; 
  Chemsp(22,8)=0; Chemsp(22,9)=0; Chemsp(22,10)=0; Chemsp(22,11)=0; Chemsp(22,12)=0; Chemsp(22,13)=0; Chemsp(22,14)=0; 
  Chemsp(22,15)=0; Chemsp(22,16)=0; Chemsp(22,17)=0; Chemsp(22,18)=0; Chemsp(22,19)=0; Chemsp(22,20)=0; Chemsp(22,21)=-1.0; 
  Chemsp(22,22)=0

  Chemsp(23,1)=0; Chemsp(23,2)=0; Chemsp(23,3)=0; Chemsp(23,4)=0; Chemsp(23,5)=0; Chemsp(23,6)=0; Chemsp(23,7)=0; 
  Chemsp(23,8)=0; Chemsp(23,9)=0; Chemsp(23,10)=-1.0; Chemsp(23,11)=1.0; Chemsp(23,12)=0; Chemsp(23,13)=0.083; Chemsp(23,14)=0; 
  Chemsp(23,15)=0; Chemsp(23,16)=0; Chemsp(23,17)=0; Chemsp(23,18)=0; Chemsp(23,19)=0; Chemsp(23,20)=0; Chemsp(23,21)=0; 
  Chemsp(23,22)=0

  Chemsp(24,1)=0; Chemsp(24,2)=0; Chemsp(24,3)=0; Chemsp(24,4)=0; Chemsp(24,5)=0; Chemsp(24,6)=0; Chemsp(24,7)=0; 
  Chemsp(24,8)=0; Chemsp(24,9)=0; Chemsp(24,10)=0; Chemsp(24,11)=-1.0; Chemsp(24,12)=1.0; Chemsp(24,13)=0.083; Chemsp(24,14)=0; 
  Chemsp(24,15)=0; Chemsp(24,16)=0; Chemsp(24,17)=0; Chemsp(24,18)=0; Chemsp(24,19)=0; Chemsp(24,20)=0; Chemsp(24,21)=0; 
  Chemsp(24,22)=0

  Chemsp(25,1)=0; Chemsp(25,2)=0; Chemsp(25,3)=0; Chemsp(25,4)=0; Chemsp(25,5)=0; Chemsp(25,6)=0; Chemsp(25,7)=0; 
  Chemsp(25,8)=0; Chemsp(25,9)=0; Chemsp(25,10)=0; Chemsp(25,11)=0; Chemsp(25,12)=0; Chemsp(25,13)=1.0; Chemsp(25,14)=1.0; 
  Chemsp(25,15)=0; Chemsp(25,16)=0; Chemsp(25,17)=0; Chemsp(25,18)=0; Chemsp(25,19)=0; Chemsp(25,20)=0; Chemsp(25,21)=0; 
  Chemsp(25,22)=0

  Chemsp(26,1)=0; Chemsp(26,2)=0; Chemsp(26,3)=-1.0; Chemsp(26,4)=1.0; Chemsp(26,5)=0; Chemsp(26,6)=0; Chemsp(26,7)=0; 
  Chemsp(26,8)=0; Chemsp(26,9)=0; Chemsp(26,10)=0; Chemsp(26,11)=0; Chemsp(26,12)=0; Chemsp(26,13)=0.071; Chemsp(26,14)=0; 
  Chemsp(26,15)=0; Chemsp(26,16)=0; Chemsp(26,17)=0; Chemsp(26,18)=0; Chemsp(26,19)=0; Chemsp(26,20)=0; Chemsp(26,21)=0; 
  Chemsp(26,22)=0

  Chemsp(27,1)=0; Chemsp(27,2)=0; Chemsp(27,3)=0; Chemsp(27,4)=0; Chemsp(27,5)=0; Chemsp(27,6)=0; Chemsp(27,7)=1.0; 
  Chemsp(27,8)=-1.0; Chemsp(27,9)=0; Chemsp(27,10)=0; Chemsp(27,11)=0; Chemsp(27,12)=0; Chemsp(27,13)=0.032; Chemsp(27,14)=0; 
  Chemsp(27,15)=0; Chemsp(27,16)=0; Chemsp(27,17)=0; Chemsp(27,18)=0; Chemsp(27,19)=0; Chemsp(27,20)=0; Chemsp(27,21)=0; 
  Chemsp(27,22)=0

  Chemsp(28,1)=0; Chemsp(28,2)=0; Chemsp(28,3)=0; Chemsp(28,4)=0; Chemsp(28,5)=0; Chemsp(28,6)=0; Chemsp(28,7)=0; 
  Chemsp(28,8)=0; Chemsp(28,9)=0; Chemsp(28,10)=0; Chemsp(28,11)=0; Chemsp(28,12)=0.3; Chemsp(28,13)=0; Chemsp(28,14)=0; 
  Chemsp(28,15)=1.0; Chemsp(28,16)=0; Chemsp(28,17)=0; Chemsp(28,18)=0; Chemsp(28,19)=0; Chemsp(28,20)=0; Chemsp(28,21)=0; 
  Chemsp(28,22)=0

end subroutine matrix

end module matrixCoeff