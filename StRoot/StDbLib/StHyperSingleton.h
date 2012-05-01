#ifndef __ST_HYPERSINGLETON_H
#define __ST_HYPERSINGLETON_H

// taken from :
// http://www.devarticles.com/c/a/Cplusplus/C-plus-plus-In-Theory-The-Singleton-Pattern-Part-2/3/
//
// Meyers Singleton is an implementation that prevents memory leakage. The Gamma Singleton, on the other hand, doesn’t clean up after itself
// and this comes in handy with our destruction sequence problem. The best way to guarantee our singleton will remain alive to the very end
// is by simply not destroying it!
//

// This is how a Gamma Singleton would instantiate its object.
template <class T> struct StHyperCreateGamma {
    static T* Create() {
        return new T;
    }
};

// This is how a Meyers Singleton would instantiate its object.
template <class T> struct StHyperCreateMeyers {
    static T* Create() {
        static T _instance;
        return &_instance;
    }
};

// This Singleton class accepts different creation policies.
template <class T, template<class> class StHyperCreationPolicy=StHyperCreateMeyers>
class StHyperSingleton
{
public:
    static T& Instance() {
        if (!m_pInstance)
            m_pInstance=StHyperCreationPolicy<T>::Create();
        return *m_pInstance;
    }

private:
    StHyperSingleton();          // ctor hidden
    ~StHyperSingleton();          // dtor hidden
    StHyperSingleton(StHyperSingleton const&);    // copy ctor hidden
    StHyperSingleton& operator=(StHyperSingleton const&);  // assign op hidden

    static T* m_pInstance;
};

template <class T, template<class> class C>
T* StHyperSingleton<T,C>::m_pInstance = 0;

#endif // __ST_HYPERSINGLETON_H
// eof

