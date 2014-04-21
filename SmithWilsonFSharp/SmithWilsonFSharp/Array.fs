namespace ArrayExtensions

module Array2D = 
    /// Generates 2D indexes for use with 2D arrays and matrices
    let generatesIndexes noRows noColumns = 
        [| for i = 0 to noRows-1 do for j = 0 to noColumns-1 do yield i,j |]

    /// Converts a given 2D array into a list of lists.
    let toListOfLists (array:_[,]) = 
        [0..array.GetUpperBound(0)] 
        |> List.map (fun row -> array.[row,*] |> List.ofArray) // Note the slicer notation here.

module Array =
    /// Builds a 2D array from a 1D array given the required
    /// number of rows and columns
    let to2DArray noRows noColumns (a:_[]) = 
        let no1DArrayItems = a.Length
        let itemsIn2DArray = noRows * noColumns
        if not (no1DArrayItems = no1DArrayItems) 
        then failwithf "Supplied array must have the same number of items as the implied, destination 2D array.  Found %i items, needed %i" 
                no1DArrayItems 
                itemsIn2DArray
        Array2D.init noRows noColumns (fun i j -> a.[noRows*i + j])