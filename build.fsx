#load "node_modules/fable-publish-utils/PublishUtils.fs"
open PublishUtils

run "npm test"
match args with
| IgnoreCase "publish"::_ ->
    pushNuget "src/Fable.RemoteData.fsproj"
| _ -> ()
