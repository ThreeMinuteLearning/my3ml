var app = Elm.Main.init({flags: navigator.userAgent});

app.ports.elmToVega.subscribe(function(spec) {
    window.requestAnimationFrame(function() {
        console.log(spec);
        vegaEmbed("#vis", spec, {actions: false, renderer: 'svg'}).catch(console.warn);
    });
});
