
' untranslated and translated vertices + color
DIM vx1(8), vy1(8), vx2(8), vy2(8)
DIM tx1(8), ty1(8), tx2(8), ty2(8), wc(8) AS SINGLE

' rotated vertices
DIM rx1(8), ry1(8), rx2(8), ry2(8) AS SINGLE

DIM w AS INTEGER, nw AS INTEGER

READ nw
FOR w = 1 TO nw
    READ vx1(w), vy1(w), vx2(w), vy2(w), wc(w)
NEXT

SCREEN 13

angle = 0
delta_angle = 0.05
offset = 0

TYPE point2d
    x AS SINGLE
    y AS SINGLE
END TYPE

TYPE vector2d
    x AS SINGLE
    y AS SINGLE
END TYPE

TYPE line2d
    n AS point2d
    c AS SINGLE
END TYPE

TYPE segment
    a_world AS point2d
    b_world AS point2d
    a_view AS point2d
    b_view AS point2d
END TYPE

DIM camera AS point2d
camera.x = 60
camera.y = 60

DIM near AS point2d
near.x = 0
near.y = -1
CALL Normalize(near)

DIM view_plane_line_near AS line2d
view_plane_line_near.c = 1F
view_plane_line_near.n = near

DO

    FOR w = 1 TO nw
        tx1(w) = vx1(w) - camera.x
        ty1(w) = vy1(w) - camera.y
        tx2(w) = vx2(w) - camera.x
        ty2(w) = vy2(w) - camera.y

        rx1(w) = tx1(w) * COS(angle) + ty1(w) * SIN(angle)
        ry1(w) = ty1(w) * COS(angle) - tx1(w) * SIN(angle)
        rx2(w) = tx2(w) * COS(angle) + ty2(w) * SIN(angle)
        ry2(w) = ty2(w) * COS(angle) - tx2(w) * SIN(angle)
    NEXT

    VIEW (offset, offset)-(100 + offset, 100+offset), 0, 2

    FOR w = 1 TO nw
        IF ((-ry1(w) >= view_plane_line_near.c) OR (-ry2(w) >= view_plane_line_near.c)) THEN
            DIM pa AS point2d
            pa.x = rx1(w)
            pa.y = ry1(w)
            DIM pb AS point2d
            pb.x = rx2(w)
            pb.y = ry2(w)

            DIM p AS point2d
            DIM pn AS point2d

            IF (pa.y > pb.y) THEN
                DIM t AS point2d
                t = pa
                pa = pb
                pb = t
            END IF

            rn = IntersectSegmentLine2d(pa, pb, view_plane_line_near, pn)
            IF (rn = 1) THEN
                pb = pn
            END IF

            z1 = -pa.y
            z2 = -pb.y
            sx1 = (pa.x * view_plane_line_near.c) / z1
            sx2 = (pb.x * view_plane_line_near.c) / z2
            sy1a = (-1200 * view_plane_line_near.c) / z1
            sy1b = (1200 * view_plane_line_near.c) / z1
            sy2a = (-1200 * view_plane_line_near.c) / z2
            sy2b = (1200 * view_plane_line_near.c) / z2

            sx1 = sx1 * 100
            sx2 = sx2 * 100

            IF (sx1 > sx2) THEN
                tx = sx1
                sx1 = sx2
                sx2 = tx

                tya = sy1a
                sy1a = sy2a
                sy2a = tya

                tyb = sy1b
                sy1b = sy2b
                sy2b = tyb
            END IF

            ya = sy1a
            yb = sy1b
            ma = (sy2a - sy1a) / (sx2 - sx1)
            mb = (sy2b - sy1b) / (sx2 - sx1)

            FOR x = sx1 TO sx2
                ya = ya + ma
                yb = yb + mb
                LINE (50 + x, 0)-(50 + x, 50 + ya), 70 'ceiling
                LINE (50 + x, 50 + ya)-(50 + x, 50 + yb), 50 'wall
                LINE (50 + x, 50 + yb)-(50 + x, 50 + yb + 110), 200 'floor
            NEXT

            LINE (50 + sx1, 50 + sy1a)-(50 + sx1, 50 + sy1b), 8 'wall intersections
            LINE (50 + sx2, 50 + sy2a)-(50 + sx2, 50 + sy2b), 8

        END IF
    NEXT


    SCREEN , , page%, 1 - page%: page% = 1 - page%
    WAIT &H3DA, &H8, &H8: WAIT &H3DA, &H8

    SELECT CASE INKEY$
        CASE "a": angle = angle - delta_angle
        CASE "d": angle = angle + delta_angle

        CASE "w": camera.y = camera.y - COS(angle): camera.x = camera.x + SIN(angle)
        CASE "s": camera.y = camera.y + COS(angle): camera.x = camera.x - SIN(angle)
        CASE "q", "Q", CHR$(27): EXIT DO

    END SELECT
LOOP

SCREEN 0, 1, 0, 0: WIDTH 80, 25

FUNCTION Dot2d (a AS point2d, b AS point2d)
    Dot2d = (a.x * b.x) + (a.y * b.y)
END FUNCTION

SUB Perp (v AS point2d)
    t = v.x
    v.x = -v.y
    v.y = t
END SUB

SUB Normalize (v AS point2d)
    d = Dot2d(v, v)
    v.x = v.x / SQR(d)
    v.y = v.y / SQR(d)
END SUB

SUB MakeVector2d (a AS point2d, b AS point2d, v AS vector2d)
    v.x = (b.x - a.x)
    v.y = (b.y - a.y)
END SUB


FUNCTION IntersectSegmentLine2d (a AS point2d, b AS point2d, l AS line2d, p AS point2d)
    DIM ab AS point2d
    ab.x = b.x - a.x
    ab.y = b.y - a.y

    t = (l.c - Dot2d(l.n, a)) / Dot2d(l.n, ab)
    IF (t >= 0.0F AND t <= 1.0F) THEN
        '    p = a + t*ab
        p.x = a.x + (t * ab.x)
        p.y = a.y + (t * ab.y)
        IntersectSegmentLine2d = 1
    ELSE
        IntersectSegmentLine2d = 0
    END IF


END FUNCTION


DATA 8
DATA 50,20,70,20,14
DATA 70,20,70,50,13
DATA 70,50,90,50,12
DATA 90,50,90,80,11
DATA 90,80,70,80,11
DATA 70,80,70,100,11
DATA 70,100,50,100,11
DATA 50,100,50,20,11

