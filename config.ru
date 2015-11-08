use Rack::Static,
  :urls => ["/images", "/js", "/css"],
  :root => "examples/public"

run lambda { |env|
  [
    200,
    {
      'Content-Type'  => 'text/html',
      'Cache-Control' => 'examples/public, max-age=86400'
    },
    File.open('examples/index.html', File::RDONLY)
  ]
}
