program eigensolver
implicit none
integer, parameter :: N = 3
complex(kind=8), dimension(N,N) :: A
complex(kind=8), dimension(N) :: W
complex(kind=8), dimension(N,N) :: VR
integer :: info 
! Initialize a test matrix
A = reshape((/(1.0, 2.0), (2.0, 3.0), (-1.0, -3.0), &
(0.0, 1.0), (3.0, -1.0), (-2.0, -2.0), &
(1.0, -2.0), (2.0, -2.0), (3.0, 1.0)/), (N,N))
! Compute
! eigenvalues and
! eigenvectors
call zgeev('N', 'V', N, A, N, W, VR, N, VR, N, info) 
! Print results
if (info == 0) then
write(*,'("Eigenvalues:")')
write(*,'(2(2x,es20.16))') W
write(*,'("Eigenvectors:")')
write(*,'((2x,es20.16))') VR
else
write(*,'("Error: The zgeev routine returned with info = ",i2)') info 
endif
end program eigensolver

