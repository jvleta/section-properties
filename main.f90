program main
    use SectionProperties

    implicit none
    call circle_tests()

contains

    subroutine circle_tests()
        integer, parameter :: nsides = 10000
        integer :: i
        real, dimension(nsides) :: x, y
        real :: theta, r, dtheta
        real, parameter :: pi = 3.14159
        real :: circumference

        r = 10.0
        circumference = 2.0*pi*r
        dtheta = circumference/real(nsides - 1)
        do i = 1, nsides
            theta = (i - 1)*dtheta
            x(i) = r * cos(theta)
            y(i) = r * sin(theta)
        end do

        call driver(nsides, x, y)

    end subroutine

    subroutine triangle_tests()
        integer, parameter :: nsides = 3
        real, dimension(nsides)    :: x, y           ! Cartesian coordinates of point

        x = (/1.0, 0.5, 0.0/)
        y = (/0.0, sqrt(3.0)/2.0, 0.0/)

        call driver(nsides, x, y)

    end subroutine

    subroutine square_tests()
        integer, parameter :: nsides = 4
        real, dimension(nsides)    :: x, y           ! Cartesian coordinates of point

        x = (/1.0, 1.0, 0.0, 0.0/)
        y = (/0.0, 1.0, 1.0, 0.0/)

        call driver(nsides, x, y)

    end subroutine

    subroutine interactive_mode()
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
10          format(1x, /, 'The coordinates of vertex,', I2, ', are: ', 2(f7.3))
        end do
        !   read (*,*) dummy

        call driver(nsides, x, y)
        deallocate (x, y)
    end subroutine

end program
