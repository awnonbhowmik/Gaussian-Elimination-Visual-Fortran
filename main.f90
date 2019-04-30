real A(10,10)
open(unit=1,file='input.txt')
open(unit=2,file='output.txt')

read(1,*)N
read(1,*)((A(I,J),J=1,N+1),I=1,N)

write(2,*)
call without_pivot(A,N)
write(2,*)
call with_pivot(A,N)

close(1)
close(2)
end

subroutine without_pivot(A,N)
    real:: A(10,10),X(10)
    real M

    do I=1,N-1
        if(A(I,I)==0)then
            do J=I+1,N
                if(abs(A(J,I))>0)then
                    do K=1,N+1
                        temp=A(I,K)
                        A(I,K)=A(J,K)
                        A(J,K)=temp
                    end do
                    goto 20
                end if
            end do
        end if
20  end do

    do i=1,N-1
        if(A(I,I)==0)then
            write(2,*)'No unique solution'
            stop
        end if

        do J=I+1,N
            M=A(J,I)/A(I,I)
            do K=1,N+1
                A(J,K)=A(J,K)-M*A(I,K)
            end do
        end do
    end do

    if(A(N,N)==0)then
        write(2,*)'No unique solution'
        stop
    end if

    X(N)=A(N,N+1)/A(N,N)

    do I=N-1,1,-1
        sum_x=0
        do J=I+1,N
            sum_x=sum_x+A(I,J)*X(J)
        end do
        X(I)=(A(I,N+1)-sum_x)/A(I,I)
    end do

    write(2,*)'Solutions without pivoting:'
    do I=1,N
        write(2,*)'X = ',X(I)
    end do
    end subroutine

    
    subroutine with_pivot(A,N)
        real::A(10,10),X(10)
        real::M
        
        do I=1,N-1
            call pivot(A,N,I)
            if(A(I,I)==0)then
                write(2,*)'No unique solution'
                stop
            end if
            
            do J=I+1,N
                M=A(J,I)/A(I,I)
                do K=1,N+1
                    A(J,K)=A(J,K)-M*A(I,K)
                end do
            end do
        end do
        
        if(A(N,N)==0)then
            write(2,*)'No unique solution'
            stop
        end if

        X(N)=A(N,N+1)/A(N,N)

        do I=N-1,1,-1
            sum_x=0
            do J=I+1,N
                sum_x=sum_x+A(I,J)*X(J)
            end do
            X(I)=(A(I,N+1)-sum_x)/A(I,I)
        end do

        write(2,*)'Solutions with pivoting:'
        do I=1,N
            write(2,*)'X = ',X(I)
        end do
    end subroutine
    
    subroutine pivot(A,N,I)
        integer P
        real A(10,10),large,temp
        
        P=I
        large=abs(A(I,I))

        do J=I+1,N
            if(abs(A(J,I))>large)then
                large=abs(A(J,I))
                P=J
            end if
        end do
        
        if(P/=I)then
            do K=1,N+1
                temp=A(P,K)
                A(P,K)=A(I,K)
                A(I,K)=temp
            end do
        end if
    end subroutine