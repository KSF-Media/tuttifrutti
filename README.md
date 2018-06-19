# Tuttifrutti 🥝🍓🍒🍊🍌🍉

Tuttifrutti is a framework/helper-collection/kitchen-sink library to quickly bootstrap new (micro)services at KSF Media.

## Howto

Add it to the `packages` key in your `stack.yaml`:

```yaml
- location:
    git: https://github.com/KSF-Media/tuttifrutti
    commit: c5dd447  # Or latest commit, or release, or `master`, etc.
  extra-dep: true
```

## Candy

- **Prelude**: use `NoImplicitPrelude`, and import `Tuttifrutti.Prelude`
- **Health endpoint**: Kubernetes needs an `/healthz` endpoint, `Tuttifrutti.Health` contains `Api` and `server` for it
- **Logging**: `Tuttifrutti.Log` (and `Tuttifrutti.Log.Handle`) has all the stuff that we need to logout on `stdout` in `fluentd`/Stackdriver format with different severities
- **Http**: `Tuttifrutti.Http` (and `Tuttifrutti.Http.Handle`) has a fancy Http client integrated with `XRequestId`, `Data.Vcr`, etc.
- **Time**: `Tuttifrutti.Time` (and `Tuttifrutti.Time.Handle`) has helpers to manage the application time, and possibly freeze it in different ways (e.g. for VCR tests)
