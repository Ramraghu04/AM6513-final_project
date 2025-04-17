module arrays_declared
    include 'input.txt'
    real, allocatable :: u(:,:), v(:,:), P(:,:), rho(:,:)
    real, allocatable :: xgrid(:,:), ygrid(:,:)
end module arrays_declared

subroutine generate_grid()
 use arrays_declared
 integer :: i,j
 real :: dx,dy
 dx=Lx/Nx
 dy=Ly/Ny
 do i=1,Nx
    do j=1,Ny
        xgrid(i,j)= dx*0.5 + (i-1.0)*dx
        ygrid(i,j)= dy*0.5 + (j-1.0)*dy
    enddo
 enddo
 print*, xgrid(:,1), ygrid(:,1)
end

subroutine allocate_routine()
  use arrays_declared
  allocate(u(Nx+3,Ny+3))
  allocate(v(Nx+3,Ny+3))
  allocate(P(Nx+2,Ny+2))
  allocate(rho(Nx+2,Ny+2))
  allocate(xgrid(Nx,Ny))
  allocate(ygrid(Nx,Ny))
end

subroutine deallocate_routine()
  use arrays_declared
  deallocate(u)
  deallocate(v)
  deallocate(P)
  deallocate(rho)
  deallocate(xgrid)
  deallocate(ygrid)
end