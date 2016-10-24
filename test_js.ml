let log s = Firebug.console##log (Js.string s);;

let str = {xml|
<div><div class="json-button" onclick="callParamsJSONFromForm(&apos;test&apos;)">json</div><div class="dmw-delayed" id="dynamowbloc__1"> ⌚ </div><form action="http://localhost:8080/services/test" id="test" onsubmit="return service_form_onsubmit(&quot;test&quot;);" parameters="test.formula" target_id="dynamowbloc__1"><input name="service" type="hidden" value="test"/><input name="ctx" type="hidden" value="[[[&quot;Service_path&quot;],[&quot;Sid_path&quot;,[&quot;test&quot;]]],[[&quot;KCounters&quot;],[&quot;Counters&quot;,[[[&quot;equation&quot;,{&quot;pred&quot;:&quot;dynamowbloc__1&quot;,&quot;succ&quot;:&quot;dynamowbloc__1_out&quot;}],[&quot;section&quot;,{&quot;pred&quot;:&quot;dynamowbloc__1&quot;,&quot;succ&quot;:&quot;dynamowbloc__1_out&quot;}]],&quot;dynamowbloc__1&quot;]]],[[&quot;Section_path&quot;],[&quot;Path&quot;,[]]]]"/><label for="test.formula">formula = </label><input id="test.formula" name="formula" title="formula" type="text" value="diff(x.x.x.x, x)"/><input id="" type="submit"/></form></div>
    |xml}

let xmls = Xtmpl_xml.from_string str
let () = log
 ("Xtmpl_xml.from_string ok => "^(Xtmpl_xml.to_string xmls))
let xmls = Xtmpl_rewrite.from_xmls xmls

