module.exports = {
    defaultExtractor: content => content.match(/[A-Za-z0-9-_:/]+/g) || [],
    css: ['assets/app.css'],
    rejected: false,
    content: ['assets/index.html','assets/3ml.js','frontend/**/*.elm']
}
