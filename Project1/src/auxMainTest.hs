import Chatterbot

main :: IO ()
main = do
    print (singleWildcardMatch "*do" "bdo")
    print (singleWildcardMatch "*do" "dobedo")
    print (singleWildcardMatch "*do" "bedobe")

    print (longerWildcardMatch "*do" "bdo")
    print (longerWildcardMatch "*do" "dobedo")
    print (longerWildcardMatch "*do" "bedobe")

    print (match '*' "*do" "bdo")
    print (match '*' "*do" "dobedo")
    print (match '*' "*do" "bedobe")