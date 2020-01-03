program SectionProperties2

  implicit none

  !*************************************************************************************
  !
  !  DATA DICTIONARY
  !
  !**************************************************************************************
  integer :: n, i, dummy      ! Number of sides to polygon
  integer :: status
  real, allocatable, dimension(:)    :: x,y           ! Cartesian coordinates of point
  real, allocatable, dimension(:)    :: r,angle       ! Radius, included angle of arc 
  real                  :: area_                      ! Area of polygon
  real                  :: xbar                       ! x coordinate of centroid (wrt origin)
  real                  :: ybar                       ! y coordinate of centroid (wrt origin)
  real                  :: Ixx_                       ! Moment of intertia about horizontal axis through ybar
  real                  :: Iyy_                       ! Moment of intertia about vertical axis through xbar
  real                  :: Pxy_                       ! Product of inertia about xbar,ybar

  ! Read data from console

  write (*,*)  "Input the number of sides (minimum of three): "
  read (*,*)n

  allocate ( x(n), y(n), stat=status )

  do i=1,n
     write (*,*) "Input the x, y coordinate of vertext ",i,": "
     read (*,*) x(i),y(i)
  end do

  do i=1,n
     write (*,10) i,x(i),y(i)
10   format(1x,/,'The coordinates of vertex,',I2,', are: ',2(f7.3))
  end do
  !   read (*,*) dummy

  area_ = area(n, x, y)
  xbar = barx(n, x, y, area_)
  ybar = bary(n, x, y, area_)
  Ixx_ = Ixx(n, x, y, ybar)
  Iyy_ = Iyy(n, x, y, xbar)
  Pxy_ = Pxy(n, x, y, xbar, ybar)

  write(*,20) area_
20 format(1x,/,'The area of the section is: ',f8.3)
  write(*,30) xbar,ybar
30 format(1x,/,'The centroid the section is: ',f8.3,',' f8.3)
  write(*,40) Ixx_
40 format(1x,/,'The moment of inertia Ixx of the section about the centroid is: ',f8.3)
  write(*,50)Iyy_
50 format(1x,/,'The moment of inertia Iyy of the section about the centroid is: ',f8.3)
  write(*,60)Pxy_
60 format(1x,/,'The product of inertia Pxy of the section about the centroid is: ',f8.3)
  read (*,*) dummy

  deallocate (x, y)

contains

  real function area(n, x, y)
    implicit none
    integer, intent(in) :: n
    real, dimension(n), intent(in) :: x,y
    integer :: i
    area = 0.0
    do i=1,n-1
       area = area + (y(i+1) - y(i))*(x(i+1) + x(i))
    end do
    area = area + (y(1)-y(n))*(x(1)+x(n))
    area = area * 0.5
  end function area

  real function barx(n, x, y, area_)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: area_
    real, dimension(n), intent(in) :: x,y
    integer :: i
    barx = 0.0
    do i=1,n-1
       barx = barx + (y(i+1) - y(i))*(x(i+1)**2 + x(i)**2 + x(i+1)*x(i))
    end do
    barx = barx + (y(1)-y(n))*(x(1)**2+x(n)**2+x(1)*x(n))
    barx = barx /(6.*area_)
  end function barx

  real function bary(n, x, y, area_)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: area_
    real, dimension(n), intent(in) :: x,y
    integer :: i
    bary = 0.0
    do i=1,n-1
       bary = bary + (x(i+1) - x(i))*(y(i+1)**2 + y(i)**2 + y(i+1)*y(i))
    end do
    bary = bary + (x(1)-x(n))*(y(1)**2+y(n)**2+y(1)*y(n))
    bary = -bary /(6.*area_)
  end function bary

  real function Ixx(n, x, y, ybar)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: ybar
    real, dimension(n), intent(in) :: x,y
    integer :: i
    Ixx = 0.0
    do i=1,n-1
       Ixx = Ixx + (x(i+1) - x(i))*(0.5*(y(i+1)+y(i))*(y(i+1)**2 + y(i)**2) &
            & -2*ybar*(y(i+1)**2 + y(i)**2 + y(i+1)*y(i)) &
            & +3*ybar**2*(y(i+1)+y(i)))
    end do
    Ixx = Ixx + (x(1) - x(n))*(0.5*(y(1)+y(n))*(y(1)**2 + y(n)**2) &
         & -2*ybar*(y(1)**2 + y(n)**2 + y(1)*y(n)) &
         & +3*ybar**2*(y(1)+y(n)))
    Ixx = -Ixx /(6.)
  end function Ixx

  real function Iyy(n, x, y, xbar)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: xbar
    real, dimension(n), intent(in) :: x,y
    integer :: i
    Iyy = 0.0
    do i=1,n-1
       Iyy = Iyy + (y(i+1) - y(i))*(0.5*(x(i+1)+x(i))*(x(i+1)**2 + x(i)**2) &
            & -2*xbar*(x(i+1)**2 + x(i)**2 + x(i+1)*x(i)) &
            & +3*xbar**2*(x(i+1)+x(i)))
    end do
    Iyy = Iyy + (y(1) - y(n))*(0.5*(x(1)+x(n))*(x(1)**2 + x(n)**2) &
         & -2*xbar*(x(1)**2 + x(n)**2 + x(1)*x(n)) &
         & +3*xbar**2*(x(1)+x(n)))
    Iyy = Iyy /(6.)
  end function Iyy

  real function Pxy(n, x, y, xbar, ybar)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: xbar, ybar
    real, dimension(n), intent(in) :: x,y
    integer :: i
    real :: dx,dy, dy_dx
    Pxy = 0.0
    do i=1,n-1
       dx = x(i+1) - x(i)
       dy = y(i+1) - y(i)
       if (dy == 0.) then ! horizontal line
          Pxy = Pxy - ( x(i+1)**2 * y(i+1)**2 - x(i)**2 * y(i)**2 )/8. 
       else if (dx == 0.) then ! vertical line
          Pxy = Pxy + ( x(i+1)**2 * y(i+1)**2 - x(i)**2 * y(i)**2 )/8.
       else       
          dy_dx = dy/dx
          Pxy = Pxy - (1./8.)* ( y(i)-dy_dx*x(i) )**2  * ( x(i+1)**2 - x(i)**2 )&
               -(1./12.)* dy_dx * ( y(i) - dy_dx * x(i) )* ( x(i+1)**3 - x(i)**3)
       end if
    end do

    ! Last Segment

    dx = x(1) - x(n)
    dy = y(1) - y(n)
    if (dy == 0.) then ! horizontal line
       Pxy = Pxy -  (x(1)**2)*((y(1)**2) - (x(n)**2)*(y(n)**2) )/8. 
    else if (dx == 0.) then ! vertical line
       Pxy = Pxy +  (x(1)**2)*((y(1)**2) - (x(n)**2)*(y(n)**2) )/8.
    else       
       dy_dx = dy/dx
       Pxy = Pxy - (1./8.)* ( (y(n)-dy_dx*x(n))**2 ) * ( x(1)**2 - x(n)**2 )&
            -(1./12.)* dy_dx* ( y(n) - dy_dx * x(n) )* ( x(1)**3 - x(n)**3) 
    end if
    Pxy = Pxy - xbar*ybar*area_

  end function Pxy

end program SectionProperties2
