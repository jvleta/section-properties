program main
    use SectionProperties

    implicit none

    integer :: nsides
    integer :: i      ! Number of sides to polygon
    real, allocatable, dimension(:)    :: x, y           ! Cartesian coordinates of point
    
    ! Read data from console

    write (*, *) "Input the number of sides (minimum of three): "
    read (*, *) nsides

    allocate (x(nsides), y(nsides))

    do i = 1, nsides
        write (*, *) "Input the x, y coordinate of vertext ", i, ": "
        read (*, *) x(i), y(i)
    end do

    do i = 1, nsides
        write (*, 10) i, x(i), y(i)
10      format(1x, /, 'The coordinates of vertex,', I2, ', are: ', 2(f7.3))
    end do
    !   read (*,*) dummy

    call driver(nsides, x, y)
    deallocate (x, y)

end program
