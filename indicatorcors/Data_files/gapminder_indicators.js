//setTimeout ( "createTable()", 500 );

function createTable() {

var tableData = [];
var table_html = "<table class='display' id='indicators-table'>";
table_html += "<thead><tr><th>Indicator name</th><th>Data provider</th><th>Category</th><th>Subcategory</th><th>Download&nbsp;&nbsp;&nbsp;View&nbsp;&nbsp;&nbsp;Visualize</th></tr></thead><tbody>";
table_html += "</tbody></table>";

jQuery.each(indicatorsJson, function(key, entry){
  var download_link = "http://spreadsheets.google.com/pub?key=" + key + "&output=xls";
  var view_link = "http://spreadsheets.google.com/pub?key=" + key + "&gid=0";

  // if (entry.googlenewurlformat) {
  //   var download_link = "https://docs.google.com/spreadsheets/d/" + key + "/export?format=xls";
  //   var view_link = "https://docs.google.com/spreadsheets/d/" + key + "/pubhtml?gid=0";
  // }

  var see_in_gw_link = "/world/#$majorMode=chart$is;shi=t;ly=2003;lb=f;il=t;fs=11;al=30;stl=t;st=t;nsl=t;se=t$wst;tts=C$ts;sp=5.59290322580644;ti=2010$zpv;v=0$inc_x;mmid=XCOORDS;iid=phAwcNAVuyj1jiMAkmq1iMg;by=ind$inc_y;mmid=YCOORDS;iid=" + key + ";by=ind$inc_s;uniValue=8.21;iid=phAwcNAVuyj0XOoBL_n5tAQ;by=ind$inc_c;uniValue=255;gid=CATID0;by=grp$map_x;scale=log;dataMin=194;dataMax=96846$map_y;scale=lin;sma=49;smi=2.65$cd;bd=0$inds=";

  var dataprovider = "<a class='openRow' rel='" + key + "'>" + entry.dataprovider + "</a>";
  var indicatorName = "<a class='openRow' rel='" + key + "'>" + entry.indicatorName + "</a>";


  var category = entry.category;
  var subcategory = entry.subcategory;

  var actions = "<a target='_blank' href=" + download_link  + "><img class='icon' src='http://www.gapminder.org/GapminderMedia/wp-uploads/data_icons/excel.png' /></a> <a target='_blank' href=" + view_link  + "><img class='icon' src='http://www.gapminder.org/GapminderMedia/wp-uploads/data_icons/view.png' /></a> <a target='_blank' href=" + see_in_gw_link  + "><img class='icon' src='http://www.gapminder.org/GapminderMedia/wp-uploads/data_icons/visualize.png' /></a>";

  //href:"http://spreadsheets.google.com/pub?key=pyj6tScZqmEdIphYUHxcdLg&range=A1:D172&gid=1"
  //<iframe frameborder="0" marginwidth="0" marginheight="0" border="0" style="border:0;margin:0;width:720px;height:360px;" src="http://spreadsheets.google.com/pub?output=html&amp;widget=true&amp;single=true&amp;element=true&amp;gid=0&amp;key=rNB2i1-CVSmpCxOX-AtJjJA" scrolling="no" allowtransparency="true"></iframe>

  tableData.push ([indicatorName, dataprovider, category, subcategory, actions]);


});


jQuery("#indicators-content").html(table_html);


var oTable;

oTable = jQuery('#indicators-table').dataTable({
    "aaData" : tableData,
    "bJQueryUI": true,
    "sPaginationType": "full_numbers",
    //"bStateSave": true,
    "iDisplayLength": 25,
    "sDom": '<"H"l<"provider-filter">fr>t<"F"ip>',
    "oLanguage": {
      "sLengthMenu": 'Show <select><option value="10">10</option><option value="25">25</option><option value="50">50</option><option value="100">100</option><option value="-1">All</option></select> indicators'
    },
    "aoData": [{ "sType": "html" },{ "sType": "html" },null,null,null],
    "aoColumns": [
          { "sTitle": "Indicator name" },
          { "sTitle": "Data provider" },
          { "sTitle": "Category" },
          { "sTitle": "Subcategory", "sClass": "center" },
          { "sTitle": "Download&nbsp;&nbsp;&nbsp;View&nbsp;&nbsp;&nbsp;Visualize","bSortable": false}
        ]


    //[null,null,null,null,{ "bSortable": false}]
});


jQuery('td a.openRow').live("click", function() {
  var key = jQuery(this).attr("rel");

  jQuery.colorbox({
    width:"950px",
    height:"600px",
    opacity: 0.6,
    transition: "none",
    iframe: true,
    href: "http://spreadsheets.google.com/pub?key=" + key + "&range=A1:D70&gid=1&output=html",
    onComplete: function() {
      setTimeout(function() {
        jQuery('.loading-bar').hide();
      }, 500);
    }
  });

  var html_to_prepend = "<h1>Indicator summary</h1><p>The table below contains a summary of the indicator and, where applicable, links to the data provider and documentation.</p>";
  html_to_prepend += "<div class='loading-bar'><img src='http://www.gapminder.org/GapminderMedia/wp-uploads/images_loaders/ajax-loader.gif' width='220' height='19'></div>";
  jQuery("#cboxContent").prepend(html_to_prepend);
  jQuery("#cboxIframe").height("400px");

  jQuery('#cboxIframe').load(function()
     {
         jQuery('.loading-bar').hide();
     });

});

}

createTable();


