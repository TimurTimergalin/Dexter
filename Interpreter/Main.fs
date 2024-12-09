module Interpreter.Main

open System.Collections.Generic
open System.IO
open System.Text
open FParsec
open Interpreter.Builtins.Builtins
open Interpreter.Evaluate
open Interpreter.Exceptions
open Interpreter.Imperative
open Interpreter.Types
open Interpreter.Value
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open Parser.Ast
open Parser.Context
open Parser.Main

type ModuleIndex =
    { modules: Map<string, InterpreterResults>
      beingProcessed: Set<string> }

let absolutePathResolver (pars: InterpreterParams) =
    let path = pars.module'

    if Path.IsPathFullyQualified path && File.Exists path then
        Some(path)
    else
        None

let localResolver from (pars: InterpreterParams) =
    let path = pars.module'
    let resPath = Path.GetFullPath(path, from)
    if File.Exists resPath then Some resPath else None

let stdResolver (pars: InterpreterParams) =
    let path = pars.module'
    let from = pars.stdPath
    let resPath = Path.GetFullPath(path, from)
    if File.Exists resPath then Some resPath else None

let libResolver (pars: InterpreterParams) =
    let path = pars.module'
    let from = pars.libPath
    let resPath = Path.GetFullPath(path, from)
    if File.Exists resPath then Some resPath else None

let rec performProgram
    (Program stmts)
    (pars: InterpreterParams)
    (index: ModuleIndex)
    : InterpreterResults * ModuleIndex =
    let closureCtx: Context = Dictionary()
    let stack = [ builtins ]
    let initRes: InterpreterResults = { context = closureCtx; result = None }

    let res =
        List.fold
            (fun (res', index') stmt ->
                match stmt with
                | Equation _ ->
                    let newCtx = performEquation stack res'.context stmt
                    let res'' = {context = newCtx; result = res'.result}

                    (res'', index')
                | Eval _ ->
                    let res'' =
                        { res' with
                            context = performEval stack res'.context stmt }

                    (res'', index')
                | TypeDeclaration _ ->
                    let res'' =
                        { res' with
                            context = performType stack res'.context stmt }

                    (res'', index')
                | Entrypoint ex ->
                    match res'.result with
                    | None ->
                        let res'' =
                            { res' with
                                result = Some(Closure(Unrecognizable ex, res'.context::stack, false)) }

                        (res'', index')
                    | Some _ -> raise (redefinitionError pars.module')
                | ImportNamespace(name, alias) ->
                    let ns, index'' =
                        loadFile
                            { pars with
                                module' = name
                                run = false }
                            pars.module'
                            index'

                    let aliasName =
                        match alias with
                        | None ->
                            let withExt = Path.GetFileName name
                            let splitArray = withExt.Split '.'
                            splitArray[splitArray.Length - 1]
                        | Some(NamespacedName(_, Identifier(name'))) -> name'

                    if res'.context.ContainsKey(aliasName) then
                        raise (incorrectNameBinding aliasName)

                    let res'' =
                        { res' with
                            context = withSet res'.context aliasName (Namespace ns.context) }

                    (res'', index'')
                | ImportAll(name, alias) ->
                    let ns, index'' =
                        loadFile
                            { pars with
                                module' = name
                                run = false }
                            pars.module'
                            index'

                    let ctx =
                        match combine2 res'.context ns.context with
                        | Ok(ctx') -> ctx'
                        | Error key -> raise (incorrectNameBinding key)

                    let ctx' =
                        match alias with
                        | None -> ctx
                        | Some(NamespacedName(_, Identifier(aliasName))) ->
                            if res'.context.ContainsKey(aliasName) then
                                raise (incorrectNameBinding aliasName)

                            withSet ctx aliasName (Namespace ns.context)

                    let res'' = { res' with context = ctx' }
                    (res'', index'')
                | _ -> (res', index'))
            (initRes, index)
            stmts

    res

and loadFile (pars: InterpreterParams) (from: string) (index: ModuleIndex) : InterpreterResults * ModuleIndex =
    let path = pars.module'
    let extension = Path.GetExtension(path)

    if extension <> ".dxt" then
        raise (invalidExtension path)

    let resolvers =
        [ absolutePathResolver; localResolver from; stdResolver; libResolver ]

    let rec getFile =
        function
        | [] -> raise (sourceNotFound path)
        | resolver :: rest ->
            match resolver pars with
            | Some path' -> path'
            | None -> getFile rest

    let resolvedPath = getFile resolvers

    if index.beingProcessed.Contains resolvedPath then
        raise (circularImport resolvedPath)

    if index.modules.ContainsKey resolvedPath then
        (index.modules[resolvedPath], index)
    else

        let parsingRes = runParserOnFile program initContext resolvedPath (UTF8Encoding())

        let program =
            match parsingRes with
            | Success(node, _, _) -> node
            | ParserResult.Failure(msg, _, _) -> raise (SyntaxError msg)

        let pars' = { pars with module' = resolvedPath }

        let result, index' =
            performProgram
                program
                pars'
                { index with
                    beingProcessed = Set.add resolvedPath index.beingProcessed }

        (result,
         { beingProcessed = Set.remove resolvedPath index'.beingProcessed
           modules = Map.add resolvedPath result index'.modules })

let runDexter filename stdPath libPath =
    builtinsInit ()
    let path = (Path.GetFullPath filename)

    let pars: InterpreterParams =
        { run = true
          module' = path
          stdPath = Path.GetFullPath stdPath
          libPath = Path.GetFullPath libPath }

    let res, _ =
        loadFile
            pars
            path
            { modules = Map.empty
              beingProcessed = Set.empty }
    
    match res.result with
    | None -> ()
    | Some expr -> evalAll [] expr |> ignore
