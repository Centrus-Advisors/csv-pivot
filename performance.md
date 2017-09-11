
- Unwrapping ByteString before using default writeFile

        real    12.020s
        user    19.952s
        sys     12.768s


- Using lazy ByteString write

        real	6.162s
        user	8.596s
        sys     4.588s

- Not using strings at all in the result list

        real	5.403s
        user	7.552s
        sys     4.584s

- Use strict Vector.foldl

        real	5.576s
        user	7.836s
        sys     4.456s

- Using foldMap

        real	7.903s
        user	12.128s
        sys     8.020s
