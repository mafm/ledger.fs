module ExcelOutput

open InputTypes
open TextOutput
open InternalTypes
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

let setIndent (ws : ExcelWorksheet) (row: int) (indent: int) =
    ws.Row(row).OutlineLevel <- (indent)

let collapse (ws : ExcelWorksheet) (row: int) =
    ws.Row(row).Collapsed <- true

type Excel =
    static member setValue((cell: ExcelRangeBase),(str: string)) =
        cell.Value <- str

    static member setValue((cell: ExcelRangeBase),(a: Amount)) =
        match a with
        | AUD a ->
            cell.Value <- (0.01 * (float a))
            cell.Style.Numberformat.Format <- "\"$\"#,##0.00;[Red]-\"$\"#,##0.00"

    static member write((report : ReportRegister.Report), (destination : Destination)) =
        match destination with
            | None  -> ()
            | Some package ->
                let worksheet = package.Workbook.Worksheets.Add("Running Balance - " + report.account.AsString)
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
                    Excel.setValue(worksheet.Cells.[rowCount, 4], line.account.AsString)
                    Excel.setValue(worksheet.Cells.[rowCount, 5], line.description)
                    rowCount <- rowCount + 1
                for c in 1..5 do
                    worksheet.Column(c).AutoFit(0.0)

    static member depthBalancesLine((line : ReportBalancesByDate.Line)) =
        1 + (List.fold max 0 (List.map Excel.depthBalancesLine line.SubAccounts))

    /// How deeply are the accounts in this report nested?
    static member depthBalances((report : ReportBalancesByDate.Report)) =
        (List.fold max 0 (List.map Excel.depthBalancesLine report.Lines))

    /// XXX: duplicate code
    static member depthBalanceSheetLine((line : ReportBalanceSheet.Line)) =
        1 + (List.fold max 0 (List.map Excel.depthBalanceSheetLine line.SubAccounts))

    /// How deeply are the accounts in this report nested?
    /// XXX: duplicate code
    static member depthBalanceSheet((report : ReportBalanceSheet.Report)) =
        (List.fold max 0 (List.map Excel.depthBalanceSheetLine report.Lines))

    /// XXX: duplicate code
    static member depthProfitAndLossLine((line : ReportProfitAndLoss.Line)) =
        1 + (List.fold max 0 (List.map Excel.depthProfitAndLossLine line.SubAccounts))

    /// How deeply are the accounts in this report nested?
    /// XXX: duplicate code
    static member depthProfitAndLoss((report : ReportProfitAndLoss.Report)) =
        (List.fold max 0 (List.map Excel.depthProfitAndLossLine report.Lines))

    static member writeLine((line: ReportBalancesByDate.Line), (dates: Date list), (ws : ExcelWorksheet), (indent: int), (nextRow: int), (txnCol:int)) =

        let writePosting (d: PostingDetail) (nextRow: int) =
            setIndent ws nextRow (indent+1)

            let numDates = dates.Length
            let mutable col = numDates
            for date in dates do
                if d.transaction.date <= date then
                    Excel.setValue (ws.Cells.[nextRow, col], d.posting.amount)
                    ws.Cells.[nextRow, col].Style.Font.Italic <- true
                col <- col - 1

            col <- (2*numDates)
            match dates with
            | first::rest -> let mutable prevDate = first
                             for date in rest do
                                if ((prevDate < d.transaction.date) && (d.transaction.date <= date)) then
                                    Excel.setValue (ws.Cells.[nextRow, col], d.posting.amount)
                                    ws.Cells.[nextRow, col].Style.Font.Italic <- true
                                col <- col - 1
                                prevDate <- date
            | [] -> ()

            Excel.setValue (ws.Cells.[nextRow, txnCol], (Text.fmtTxnId d.transaction.id))
            Excel.setValue (ws.Cells.[nextRow, txnCol+1], (Text.fmtDate d.transaction.date))
            Excel.setValue (ws.Cells.[nextRow, txnCol+2], d.transaction.description)

            ws.Cells.[nextRow, txnCol].Style.Font.Italic <- true
            ws.Cells.[nextRow, txnCol+1].Style.Font.Italic <- true
            ws.Cells.[nextRow, txnCol+2].Style.Font.Italic <- true
            (collapse ws nextRow)

            nextRow+1

        let rec writePostings (postings: PostingDetail List) (nextRow: int) =
            match postings with
            | [] -> nextRow
            | first::rest ->
                writePostings rest (writePosting first nextRow)

        let numDates = dates.Length
        let mutable column = numDates

        // Write as-at balances
        (setIndent ws nextRow indent)
        for balance in line.Amounts.Balances do
            (Excel.setValue (ws.Cells.[nextRow, column], balance))
            column <- column - 1

        // Write differences between dates
        column <- (numDates * 2)
        for difference in line.Amounts.Differences do
            (Excel.setValue (ws.Cells.[nextRow, column], difference))
            column <- column - 1

        column <- 2*(numDates+1)

        Excel.setValue (ws.Cells.[nextRow, column+indent], line.Account.AsString)
        let rowAfterChildren = Excel.writeLines (line.SubAccounts, dates, ws, indent+1, (nextRow+1), txnCol)
        let rowAfterPostings = (writePostings line.Postings rowAfterChildren)
        (collapse ws nextRow)
        rowAfterPostings

    static member writeLines((lines : ReportBalancesByDate.Line list),(dates: Date list), (ws : ExcelWorksheet),  (indent: int), (nextRow: int), (txnCol:int)) =
        match lines with
            | [] -> nextRow
            | first::rest -> Excel.writeLines(rest, dates, ws, indent, Excel.writeLine(first, dates, ws, indent, nextRow, txnCol), txnCol)

    static member write((report : ReportBalancesByDate.Report), (destination : Destination)) =
        let numDates = report.Dates.Length

        let dateIndexToBalanceColumn i =
            numDates-i

        let dateIndexToChangeColumn i =
            (2*numDates) + 1 - i

        match destination with
            | None  -> ()
            | Some package ->

                let accountDepth = Excel.depthBalances(report)
                let worksheet = package.Workbook.Worksheets.Add("Balances by Date")

                (setHeader worksheet.Cells.[1, 1] "Balance")
                for i in 0 .. (numDates-1) do
                    (setHeader worksheet.Cells.[2, dateIndexToBalanceColumn(i)] report.Dates.[i])

                (setHeader worksheet.Cells.[1, numDates+2] "Change")
                for i in 1 .. (numDates-1) do
                    (setHeader worksheet.Cells.[2, dateIndexToChangeColumn(i)] report.Dates.[i])

                (setHeader worksheet.Cells.[2, numDates*2+2] "Account")

                let txnNumCol = numDates*2+2+accountDepth
                (setHeader worksheet.Cells.[2, txnNumCol] "Transaction#")
                (setHeader worksheet.Cells.[2, txnNumCol+1] "Date")
                (setHeader worksheet.Cells.[2, txnNumCol+2] "Description")

                Excel.writeLines(report.Lines, report.Dates, worksheet,  0, 3, txnNumCol) |> ignore
                worksheet.View.FreezePanes(3, 1)
                worksheet.OutLineSummaryBelow <- false
                for c in 1..numDates do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (numDates+2)..(numDates*2) do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (numDates+2)..(numDates*2) do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (txnNumCol-1)..(txnNumCol+2) do
                    worksheet.Column(c).AutoFit(0.0)

    static member writeLine((line: ReportBalanceSheet.Line), (dates: Date list), (ws : ExcelWorksheet), (indent: int), (nextRow: int), (txnCol:int)) =

        let writePosting (d: PostingDetail) (nextRow: int) =
            setIndent ws nextRow (indent+1)

            let numDates = dates.Length
            let mutable col = numDates
            for date in dates do
                if d.transaction.date <= date then
                    Excel.setValue (ws.Cells.[nextRow, col], d.posting.amount)
                    ws.Cells.[nextRow, col].Style.Font.Italic <- true
                col <- col - 1

            Excel.setValue (ws.Cells.[nextRow, txnCol], (Text.fmtTxnId d.transaction.id))
            Excel.setValue (ws.Cells.[nextRow, txnCol+1], (Text.fmtDate d.transaction.date))
            Excel.setValue (ws.Cells.[nextRow, txnCol+2], d.transaction.description)

            ws.Cells.[nextRow, txnCol].Style.Font.Italic <- true
            ws.Cells.[nextRow, txnCol+1].Style.Font.Italic <- true
            ws.Cells.[nextRow, txnCol+2].Style.Font.Italic <- true
            (collapse ws nextRow)

            nextRow+1

        let rec writePostings (postings: PostingDetail List) (nextRow: int) =
            match postings with
            | [] -> nextRow
            | first::rest ->
                writePostings rest (writePosting first nextRow)

        let numDates = dates.Length
        let mutable column = numDates

        let rowAfterChildren = Excel.writeLines (line.SubAccounts, dates, ws, indent+1, nextRow, txnCol)
        let rowAfterPostings = (writePostings line.Postings rowAfterChildren)

        // Write as-at balances
        (setIndent ws rowAfterPostings indent)
        for balance in line.Amounts.Balances do
            (Excel.setValue (ws.Cells.[rowAfterPostings, column], balance))
            column <- column - 1

        column <- (numDates+2)

        Excel.setValue (ws.Cells.[rowAfterPostings, column+indent], line.Account.AsString)

        (collapse ws rowAfterPostings)
        rowAfterPostings+1

    static member writeLines((lines : ReportBalanceSheet.Line list), (dates: Date list), (ws : ExcelWorksheet),  (indent: int), (nextRow: int), (txnCol:int)) =
        match lines with
            | [] -> nextRow
            | first::rest ->
                let nextRow = Excel.writeLine(first, dates, ws, indent, nextRow, txnCol)
                Excel.writeLines(rest, dates, ws, indent, nextRow, txnCol)

    static member write((report : ReportBalanceSheet.Report), (destination : Destination)) =
        let numDates = report.Dates.Length

        let dateIndexToBalanceColumn i =
            numDates-i

        match destination with
            | None  -> ()
            | Some package ->

                let accountDepth = Excel.depthBalanceSheet(report)
                let worksheet = package.Workbook.Worksheets.Add("Balance Sheet")

                for i in 0 .. (numDates-1) do
                    (setHeader worksheet.Cells.[1, dateIndexToBalanceColumn(i)] report.Dates.[i])

                (setHeader worksheet.Cells.[1, numDates+2] "Account")

                let txnNumCol = numDates+2+accountDepth
                (setHeader worksheet.Cells.[1, txnNumCol] "Transaction#")
                (setHeader worksheet.Cells.[1, txnNumCol+1] "Date")
                (setHeader worksheet.Cells.[1, txnNumCol+2] "Description")

                Excel.writeLines(report.Lines, report.Dates, worksheet,  0, 2, txnNumCol) |> ignore
                worksheet.View.FreezePanes(2, 1)
                worksheet.OutLineSummaryBelow <- true
                for c in 1..numDates do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (numDates+2)..(numDates*2) do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (numDates+2)..(numDates*2) do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (txnNumCol-1)..(txnNumCol+2) do
                    worksheet.Column(c).AutoFit(0.0)

    static member writeLine((line: ReportProfitAndLoss.Line), (dates: Date list), (ws : ExcelWorksheet), (indent: int), (nextRow: int), (txnCol:int)) =

        let writePosting (d: PostingDetail) (nextRow: int) =
            setIndent ws nextRow (indent+1)

            let numDates = dates.Length
            let mutable col = numDates

            let mutable prevDate = None
            for date in dates do
                if d.transaction.date <= date then
                    let includeAtThisDate =
                            match prevDate with
                                    | None -> true
                                    | Some prevDate -> (prevDate < d.transaction.date)
                    if includeAtThisDate then
                        Excel.setValue (ws.Cells.[nextRow, col], d.posting.amount)
                        ws.Cells.[nextRow, col].Style.Font.Italic <- true
                prevDate <- Some date
                col <- col - 1

            Excel.setValue (ws.Cells.[nextRow, txnCol], (Text.fmtTxnId d.transaction.id))
            Excel.setValue (ws.Cells.[nextRow, txnCol+1], (Text.fmtDate d.transaction.date))
            Excel.setValue (ws.Cells.[nextRow, txnCol+2], d.transaction.description)

            ws.Cells.[nextRow, txnCol].Style.Font.Italic <- true
            ws.Cells.[nextRow, txnCol+1].Style.Font.Italic <- true
            ws.Cells.[nextRow, txnCol+2].Style.Font.Italic <- true
            (collapse ws nextRow)

            nextRow+1

        let rec writePostings (postings: PostingDetail List) (nextRow: int) =
            match postings with
            | [] -> nextRow
            | first::rest ->
                writePostings rest (writePosting first nextRow)

        let rowAfterChildren = Excel.writeLines (line.SubAccounts, dates, ws, indent+1, (nextRow), txnCol)
        let rowAfterPostings = (writePostings line.Postings rowAfterChildren)

        let numDates = dates.Length
        let mutable column = numDates

        (setIndent ws rowAfterPostings indent)
        // Write differences to date
        for difference in line.Amounts.Differences do
            (Excel.setValue (ws.Cells.[rowAfterPostings, column], difference))
            column <- column - 1

        column <- (numDates+2)

        Excel.setValue (ws.Cells.[rowAfterPostings, column+indent], line.Account.AsString)


        (collapse ws rowAfterPostings)
        rowAfterPostings+1

    static member writeLines((lines : ReportProfitAndLoss.Line list), (dates: Date list), (ws : ExcelWorksheet),  (indent: int), (nextRow: int), (txnCol:int)) =
        match lines with
            | [] -> nextRow
            | first::rest ->
                let nextRow = Excel.writeLine(first, dates, ws, indent, nextRow, txnCol)
                Excel.writeLines(rest, dates, ws, indent, nextRow, txnCol)

    static member write((report : ReportProfitAndLoss.Report), (destination : Destination)) =
        let numDates = report.Dates.Length

        let dateIndexToBalanceColumn i =
            numDates-i

        match destination with
            | None  -> ()
            | Some package ->

                let accountDepth = Excel.depthProfitAndLoss(report)
                let worksheet = package.Workbook.Worksheets.Add("Profit and Loss")

                for i in 0 .. (numDates-1) do
                    (setHeader worksheet.Cells.[1, dateIndexToBalanceColumn(i)] report.Dates.[i])

                (setHeader worksheet.Cells.[1, numDates+2] "Account")

                let txnNumCol = numDates+2+accountDepth
                (setHeader worksheet.Cells.[1, txnNumCol] "Transaction#")
                (setHeader worksheet.Cells.[1, txnNumCol+1] "Date")
                (setHeader worksheet.Cells.[1, txnNumCol+2] "Description")

                Excel.writeLines(report.Lines, report.Dates, worksheet,  0, 2, txnNumCol) |> ignore
                worksheet.View.FreezePanes(2, 1)
                worksheet.OutLineSummaryBelow <- true
                for c in 1..numDates do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (numDates+2)..(numDates*2) do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (numDates+2)..(numDates*2) do
                    worksheet.Column(c).AutoFit(0.0)
                for c in (txnNumCol-1)..(txnNumCol+2) do
                    worksheet.Column(c).AutoFit(0.0)

    static member writeLine((line: ReportBalances.Line),
                            (ws : ExcelWorksheet),
                            (indent: int),
                            (nextRow: int)) =
      (Excel.setValue (ws.Cells.[nextRow, 1], line.Balance))
      Excel.setValue (ws.Cells.[nextRow, 2+indent], line.Account.AsString)
      if indent <> 0 then
        ws.Row(nextRow).OutlineLevel <- (indent)
      let rowAfterChildren = Excel.writeLines (line.SubAccounts, ws, indent+1, nextRow+1)
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
      Excel.setValue (ws.Cells.[nextRow, 1+indent], line.Account.AsString)
      if indent <> 0 then
        ws.Row(nextRow).OutlineLevel <- (indent)
      // Deliberately avoid collapsing hierarchy. If we're looking, we probably want to
      // emphasise details, and it's easy to manually hide them if that's what is wanted.
      Excel.writeLines (line.SubAccounts, ws, indent+1, nextRow+1)

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
                Excel.writeLines(report.Lines, worksheet, 0, 2) |> ignore
                worksheet.View.FreezePanes(2, 1)
                worksheet.OutLineSummaryBelow <- false

    static member writeLine((line: ReportTransactionList.Line),
                            (ws : ExcelWorksheet),
                            (nextRow: int)) =
      let mutable nextRow = nextRow

      let txnCell = ws.Cells.[nextRow, 1]
      let dateCell = ws.Cells.[nextRow, 2]
      let descCell = ws.Cells.[nextRow, 3]

      Excel.setValue (txnCell, (sprintf "txn:%d" line.transaction.id))
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
        Excel.setValue (ws.Cells.[nextRow, 3], p.account.AsString)
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
