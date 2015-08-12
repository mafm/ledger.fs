﻿module ExcelOutput

open InputTypes
open TextOutput
open OfficeOpenXml

type Destination = ExcelPackage option

let newFile (filename:string) =
    let f = new System.IO.FileInfo(filename)
    if f.Exists then
        f.Delete()
        new System.IO.FileInfo(filename)
    else
        f

let destination (filename: string) =
    match filename with
    | "" -> None
    | arg -> let filename = if arg.EndsWith(".xlsx") then arg else (arg + ".xlsx")
             let file = newFile (filename)
             let package = new ExcelPackage(file)
             Some package

let save (destination : Destination) =
    match destination with
    | None -> ()
    | Some package -> package.Save()

let setHeader (cell: ExcelRangeBase) (header: string) =
    cell.Value <- header
    cell.Style.Font.Bold <- true

type Excel =
    static member setValue((cell: ExcelRangeBase),(str: string)) =
        cell.Value <- str

    static member setValue((cell: ExcelRangeBase),(a: Amount)) =
        match a with
        | AUD a ->
            cell.Value <- (0.01 * (float a))
            cell.Style.Numberformat.Format <- "\"$\"#,##0.00;[Red]\"$\"#,##0.00"

    static member write((report : ReportRegister.Report), (destination : Destination)) =
        match destination with
            | None  -> ()
            | Some package ->
                let worksheet = package.Workbook.Worksheets.Add("Running Balance - " + report.account)
                let mutable rowCount = 2
                worksheet.View.ShowGridLines <- true
                (setHeader worksheet.Cells.[1, 1] "Date")
                (setHeader worksheet.Cells.[1, 2] "Amount")
                (setHeader worksheet.Cells.[1, 3] "Balance")
                (setHeader worksheet.Cells.[1, 4] "Account")
                (setHeader worksheet.Cells.[1, 5] "Description")
                worksheet.View.FreezePanes(2,1)
                for line in report.lines do
                    Excel.setValue(worksheet.Cells.[rowCount, 1], line.date)
                    Excel.setValue(worksheet.Cells.[rowCount, 2], line.amount)
                    Excel.setValue(worksheet.Cells.[rowCount, 3], line.balance)
                    Excel.setValue(worksheet.Cells.[rowCount, 4], line.account)
                    Excel.setValue(worksheet.Cells.[rowCount, 5], line.description)
                    rowCount <- rowCount + 1
                for c in 1..5 do
                    worksheet.Column(c).AutoFit(0.0)

    static member writeLine((line: ReportBalancesByDate.ReportBalancesByDateLine), (ws : ExcelWorksheet), (indent: int), (nextRow: int)) =
        let mutable column = 1
        for balance in line.amounts.balances do
            (Excel.setValue (ws.Cells.[nextRow, column], balance))
            column <- column + 1
        column <- column + 1
        for balance in line.amounts.differences do
            (Excel.setValue (ws.Cells.[nextRow, column], balance))
            column <- column + 1
        column <- column + 1
        Excel.setValue (ws.Cells.[nextRow, column+indent], line.account)
        if indent <> 0 then
            ws.Row(nextRow).OutlineLevel <- (indent)
        let rowAfterChildren = Excel.writeLines (line.subAccounts, ws, indent+1, nextRow+1)
        if rowAfterChildren <> nextRow then
            ws.Row(nextRow).Collapsed <- true
        rowAfterChildren

    static member writeLines((lines : ReportBalancesByDate.ReportBalancesByDateLine list), (ws : ExcelWorksheet), (indent: int), (nextRow: int)) =
        match lines with
            | [] -> nextRow
            | first::rest -> Excel.writeLines(rest, ws, indent, Excel.writeLine(first, ws, indent, nextRow))

    static member write((report : ReportBalancesByDate.ReportBalancesByDate), (destination : Destination)) =
        match destination with
            | None  -> ()
            | Some package ->
                let numDates = report.dates.Length
                let worksheet = package.Workbook.Worksheets.Add("Balances by Date")
                (setHeader worksheet.Cells.[1, 1] "Balance")
                for i in 1 .. numDates do
                    (setHeader worksheet.Cells.[2, i] report.dates.[i-1])
                (setHeader worksheet.Cells.[1, numDates+2] "Change")
                for i in 2 .. numDates do
                    (setHeader worksheet.Cells.[2, numDates+i] report.dates.[i-1])
                (setHeader worksheet.Cells.[2, numDates*2+2] "Account")
                Excel.writeLines(report.lines, worksheet, 0, 3) |> ignore
                worksheet.View.FreezePanes(3, 1)
                worksheet.OutLineSummaryBelow <- false
                for c in 1..numDates do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (numDates+2)..(numDates*2) do
                    worksheet.Column(c).AutoFit(0.0)

    static member writeLine((line: ReportBalances.Line),
                            (ws : ExcelWorksheet),
                            (indent: int),
                            (nextRow: int)) =
      (Excel.setValue (ws.Cells.[nextRow, 1], line.balance))
      Excel.setValue (ws.Cells.[nextRow, 2+indent], line.account)
      if indent <> 0 then
        ws.Row(nextRow).OutlineLevel <- (indent)
      let rowAfterChildren = Excel.writeLines (line.subAccounts, ws, indent+1, nextRow+1)
      if rowAfterChildren <> nextRow then
        ws.Row(nextRow).Collapsed <- true
      rowAfterChildren

    static member writeLines((lines : ReportBalances.Line list),
                             (ws : ExcelWorksheet),
                             (indent: int),
                             (nextRow: int)) =
        match lines with
            | [] -> nextRow
            | first::rest -> Excel.writeLines(rest, ws, indent, Excel.writeLine(first, ws, indent, nextRow))

    static member write((report : ReportBalances.Report), (destination : Destination)) =
        match destination with
        | None -> ()
        | Some package ->
            let worksheet = package.Workbook.Worksheets.Add("Balances")
            (setHeader worksheet.Cells.[1, 1] "Balance")
            (setHeader worksheet.Cells.[1, 2] "Account")
            Excel.writeLines(report.lines, worksheet, 0, 2) |> ignore
            worksheet.View.FreezePanes(2, 1)
            worksheet.OutLineSummaryBelow <- false

    static member writeLine((line: ReportChartOfAccounts.Line),
                            (ws : ExcelWorksheet),
                            (indent: int),
                            (nextRow: int)) =
      Excel.setValue (ws.Cells.[nextRow, 1+indent], line.account)
      if indent <> 0 then
        ws.Row(nextRow).OutlineLevel <- (indent)
      // Deliberately avoid collapsing hierarchy. If we're looking, we probably want to
      // emphasise details, and it's easy to manually hide them if that's what is wanted.
      Excel.writeLines (line.subAccounts, ws, indent+1, nextRow+1)

    static member writeLines((lines : ReportChartOfAccounts.Line list),
                             (ws : ExcelWorksheet),
                             (indent: int),
                             (nextRow: int)) =
        match lines with
            | [] -> nextRow
            | first::rest -> Excel.writeLines(rest, ws, indent, Excel.writeLine(first, ws, indent, nextRow))

    static member write((report : ReportChartOfAccounts.Report), (destination : Destination)) =
        match destination with
            | None -> ()
            | Some package ->
                let worksheet = package.Workbook.Worksheets.Add("Chart Of Accounts")
                (setHeader worksheet.Cells.[1, 1] "Account")
                Excel.writeLines(report.lines, worksheet, 0, 2) |> ignore
                worksheet.View.FreezePanes(2, 1)
                worksheet.OutLineSummaryBelow <- false

    static member writeLine((line: ReportTransactionList.Line),
                            (ws : ExcelWorksheet),
                            (nextRow: int)) =
      let mutable nextRow = nextRow

      let txnCell = ws.Cells.[nextRow, 1]
      let dateCell = ws.Cells.[nextRow, 2]
      let descCell = ws.Cells.[nextRow, 3]

      Excel.setValue (txnCell, (sprintf "txn:%d" line.id))
      txnCell.Style.Border.Top.Style <- OfficeOpenXml.Style.ExcelBorderStyle.Thin

      Excel.setValue (dateCell, line.transaction.date)
      dateCell.Style.Font.Bold <- true
      dateCell.Style.Border.Top.Style <- OfficeOpenXml.Style.ExcelBorderStyle.Thin

      Excel.setValue (descCell, line.transaction.description)
      descCell.Style.Font.Bold <- true
      descCell.Style.Font.Italic <- true
      descCell.Style.Border.Top.Style <- OfficeOpenXml.Style.ExcelBorderStyle.Thin

      nextRow <- nextRow+1

      for p in line.transaction.postings do
        Excel.setValue (ws.Cells.[nextRow, 2], p.amount)
        Excel.setValue (ws.Cells.[nextRow, 3], p.account)
        nextRow <- nextRow+1
      nextRow

    static member writeLines((lines : ReportTransactionList.Line list),
                             (ws : ExcelWorksheet),
                             (nextRow: int)) =
        match lines with
            | [] -> nextRow
            | first::rest -> Excel.writeLines(rest, ws, Excel.writeLine(first, ws, nextRow))

    static member write((report : ReportTransactionList.Report), (destination : Destination)) =
        match destination with
            | None -> ()
            | Some package ->
                let mutable nextRow = 1
                let worksheet = package.Workbook.Worksheets.Add("Transactions")

                match report.first with
                | Some date -> (setHeader worksheet.Cells.[nextRow, 1] "From:")
                               (setHeader worksheet.Cells.[nextRow, 2] date)
                               nextRow <- nextRow+1
                | None -> ()
                match report.last with
                | Some date -> (setHeader worksheet.Cells.[nextRow, 1] "To:")
                               (setHeader worksheet.Cells.[nextRow, 2] date)
                               nextRow <- nextRow+1
                | None -> ()
                (setHeader worksheet.Cells.[nextRow, 1] "Transaction#")
                (setHeader worksheet.Cells.[nextRow, 2] "Date/Amount")
                (setHeader worksheet.Cells.[nextRow, 3] "Description/Account")
                nextRow <- nextRow + 1
                worksheet.View.FreezePanes(nextRow, 1)
                Excel.writeLines(report.lines, worksheet, nextRow) |> ignore
                for c in 1..3 do
                    worksheet.Column(c).AutoFit(0.0)
