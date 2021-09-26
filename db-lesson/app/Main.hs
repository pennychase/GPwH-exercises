module Main where

import DatabaseCommands

promptAndAddUser :: IO ()
promptAndAddUser = do
    print "Enter new user name"
    userName <- getLine
    addUser userName

promptAndAddTool :: IO ()
promptAndAddTool = do
    print "Enter new tool name"
    toolName <- getLine 
    print "Enter new tool description"
    description <- getLine
    addTool toolName description

promptAndCheckout :: IO ()
promptAndCheckout = do
    print "Enter the id of the user"
    userId <- read <$> getLine 
    print "Enter the id of the tool"
    toolId <- read <$> getLine 
    checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
    print "Enter the id of the tool"
    toolId <- pure read <*> getLine 
    checkinAndUpdate toolId

performCommand :: String -> IO ()
performCommand command
    | command == "users" = printUsers >> main
    | command == "tools" = printTools >> main
    | command == "in" = printAvailable >> main
    | command == "out" = printCheckedOut >> main
    | command == "adduser" = promptAndAddUser >> main
    | command == "addtool" = promptAndAddTool >> main
    | command == "checkout" = promptAndCheckout >> main
    | command == "checkin" = promptAndCheckin >> main
    | command == "quit" = print "bye!"
    | otherwise = print "Sorry, command not found" >> main 

main :: IO ()
main = do
    print "Enter a command"
    command <- getLine
    performCommand command

