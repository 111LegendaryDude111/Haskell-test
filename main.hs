module Main where

import TaskManager
import System.IO
import Data.List
import Control.Monad
import System.Exit
import Data.Aeson
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    putStrLn "Welcome to Task Manager!"
    tasks <- loadTasks
    mainLoop tasks

loadTasks :: IO [Task]
loadTasks = do
    exists <- doesFileExist "tasks.json"
    if exists
        then do
            content <- B.readFile "tasks.json"
            case decode content of
                Just tasks -> return tasks
                Nothing -> return []
        else return []

saveTasks :: [Task] -> IO ()
saveTasks tasks = B.writeFile "tasks.json" (encode tasks)

mainLoop :: [Task] -> IO ()
mainLoop tasks = do
    putStrLn "\nAvailable commands:"
    putStrLn "1. Add task"
    putStrLn "2. List tasks"
    putStrLn "3. Complete task"
    putStrLn "4. Delete task"
    putStrLn "5. Exit"
    putStr "Enter command: "
    hFlush stdout
    cmd <- getLine
    case cmd of
        "1" -> addTask tasks
        "2" -> listTasks tasks
        "3" -> completeTask tasks
        "4" -> deleteTask tasks
        "5" -> exitSuccess
        _ -> do
            putStrLn "Invalid command!"
            mainLoop tasks

addTask :: [Task] -> IO ()
addTask tasks = do
    putStr "Enter task title: "
    hFlush stdout
    title <- getLine
    putStr "Enter task description: "
    hFlush stdout
    desc <- getLine
    putStr "Enter priority (Low/Medium/High): "
    hFlush stdout
    prio <- readLn
    putStr "Enter due date (YYYY-MM-DD): "
    hFlush stdout
    date <- getLine
    let newTask = Task 
            { taskId = if null tasks then 1 else maximum (map taskId tasks) + 1
            , title = title
            , description = desc
            , priority = prio
            , dueDate = date
            , completed = False
            }
    let updatedTasks = newTask : tasks
    saveTasks updatedTasks
    putStrLn "Task added successfully!"
    mainLoop updatedTasks

listTasks :: [Task] -> IO ()
listTasks tasks = do
    putStrLn "\nCurrent Tasks:"
    mapM_ printTask tasks
    mainLoop tasks
  where
    printTask task = putStrLn $ 
        show (taskId task) ++ ". " ++ 
        title task ++ " [" ++ show (priority task) ++ "] " ++
        "Due: " ++ dueDate task ++ 
        (if completed task then " (COMPLETED)" else "")

completeTask :: [Task] -> IO ()
completeTask tasks = do
    putStr "Enter task ID to complete: "
    hFlush stdout
    taskIdStr <- getLine
    let taskId' = read taskIdStr :: Int
    let updatedTasks = map (\t -> if taskId t == taskId' 
                                 then t { completed = True }
                                 else t) tasks
    saveTasks updatedTasks
    putStrLn "Task marked as completed!"
    mainLoop updatedTasks

deleteTask :: [Task] -> IO ()
deleteTask tasks = do
    putStr "Enter task ID to delete: "
    hFlush stdout
    taskIdStr <- getLine
    let taskId' = read taskIdStr :: Int
    let updatedTasks = filter (\t -> taskId t /= taskId') tasks
    saveTasks updatedTasks
    putStrLn "Task deleted!"
    mainLoop updatedTasks
