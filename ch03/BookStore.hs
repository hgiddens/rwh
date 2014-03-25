-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)

myInfo = Book 9870135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

data Customer = Customer {
      customerID :: CustomerID,
      customerName :: String,
      customerAddress :: Address
      } deriving (Show)
